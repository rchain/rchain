package coop.rchain.casper.helper

import cats._
import cats.data.StateT
import cats.implicits._
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.BlockDag
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Time

import scala.collection.immutable.{HashMap, HashSet}
import scala.language.higherKinds

object BlockGenerator {
  implicit val timeEff = new LogicalTime[Id]

  type StateWithChain[A] = StateT[Id, BlockDag, A]

  type BlockDagState[F[_]] = MonadState[F, BlockDag]
  def blockDagState[F[_]: Monad: BlockDagState]: BlockDagState[F] = MonadState[F, BlockDag]

  def storeForStateWithChain[F[_]: Monad](idBs: BlockStore[Id]): BlockStore[F] =
    new BlockStore[F] {
      override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
        Monad[F].pure(idBs.get(blockHash))

      override def asMap(): F[Map[BlockHash, BlockMessage]] =
        Monad[F].pure(idBs.asMap())

      override def put(f: => (BlockHash, BlockMessage)): F[Unit] =
        Monad[F].pure(idBs.put(f))

      override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
        Monad[F].pure(idBs.find(p))

      override def clear(): F[Unit] = Monad[F].pure(idBs.clear())
      override def close(): F[Unit] = Monad[F].pure(idBs.close())
    }
}

trait BlockGenerator {
  import BlockGenerator._

  def createBlock[F[_]: Monad: BlockDagState: Time: BlockStore](
      parentsHashList: Seq[BlockHash],
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[DeployCost] = Seq.empty[DeployCost],
      tsHash: ByteString = ByteString.EMPTY,
      tsLog: Seq[Event] = Seq.empty[Event],
      shardId: String = ""): F[BlockMessage] =
    for {
      chain             <- blockDagState[F].get
      now               <- Time[F].currentMillis
      nextId            = chain.currentId + 1
      nextCreatorSeqNum = chain.currentSeqNum.getOrElse(creator, -1) + 1
      postState = RChainState()
        .withTuplespace(tsHash)
        .withBonds(bonds)
        .withBlockNumber(nextId.toLong)
      postStateHash = Blake2b256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
        .withNewCodeHash(ProtoUtil.protoSeqHash(deploys))
        .withCommReductionsHash(ProtoUtil.protoSeqHash(tsLog))
        .withTimestamp(now)
      blockHash = Blake2b256.hash(header.toByteArray)
      body      = Body().withPostState(postState).withNewCode(deploys).withCommReductions(tsLog)
      serializedJustifications = justifications.toList.map {
        case (creator: Validator, latestBlockHash: BlockHash) =>
          Justification(creator, latestBlockHash)
      }
      serializedBlockHash = ByteString.copyFrom(blockHash)
      block = BlockMessage(serializedBlockHash,
                           Some(header),
                           Some(body),
                           serializedJustifications,
                           creator,
                           nextCreatorSeqNum,
                           shardId = shardId)
      idToBlocks     = chain.idToBlocks + (nextId -> block)
      _              <- BlockStore[F].put(serializedBlockHash, block)
      latestMessages = chain.latestMessages + (block.sender -> serializedBlockHash)
      latestMessagesOfLatestMessages = chain.latestMessagesOfLatestMessages + (block.sender -> ProtoUtil
        .toLatestMessages(serializedJustifications))
      updatedChildren = HashMap[BlockHash, Set[BlockHash]](parentsHashList.map {
        parentHash: BlockHash =>
          val currentChildrenHashes = chain.childMap.getOrElse(parentHash, HashSet.empty[BlockHash])
          val updatedChildrenHashes = currentChildrenHashes + serializedBlockHash
          parentHash -> updatedChildrenHashes
      }: _*)
      childMap = chain.childMap
        .++[(BlockHash, Set[BlockHash]), Map[BlockHash, Set[BlockHash]]](updatedChildren)
      updatedSeqNumbers = chain.currentSeqNum.updated(creator, nextCreatorSeqNum)
      newChain: BlockDag = BlockDag(idToBlocks,
                                    childMap,
                                    latestMessages,
                                    latestMessagesOfLatestMessages,
                                    nextId,
                                    updatedSeqNumbers)
      _ <- blockDagState[F].set(newChain)
    } yield block
}
