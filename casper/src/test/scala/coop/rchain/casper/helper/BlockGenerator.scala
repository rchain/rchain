package coop.rchain.casper.helper

import cats._
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, ProcessedDeployUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Time
import monix.eval.Task

import scala.collection.immutable.HashMap
import scala.language.higherKinds

object BlockGenerator {
  implicit val timeEff = new LogicalTime[Task]

  def updateChainWithBlockStateUpdate[F[_]: Sync: BlockStore: IndexedBlockDagStorage](
      id: Int,
      genesis: BlockMessage,
      runtimeManager: RuntimeManager[F]
  ): F[BlockMessage] =
    for {
      b   <- IndexedBlockDagStorage[F].lookupByIdUnsafe(id)
      dag <- IndexedBlockDagStorage[F].getRepresentation
      computeBlockCheckpointResult <- computeBlockCheckpoint[F](
                                       b,
                                       genesis,
                                       dag,
                                       runtimeManager
                                     )
      (postStateHash, processedDeploys) = computeBlockCheckpointResult
      _                                 <- injectPostStateHash[F](id, b, genesis, postStateHash, processedDeploys)
    } yield b

  def computeBlockCheckpoint[F[_]: Sync: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[(StateHash, Seq[ProcessedDeploy])] =
    for {
      result <- InterpreterUtil
                 .computeBlockCheckpointFromDeploys[F](b, genesis, dag, runtimeManager)
      Right((preStateHash, postStateHash, processedDeploys)) = result
    } yield (postStateHash, processedDeploys.map(ProcessedDeployUtil.fromInternal))

  def injectPostStateHash[F[_]: Monad: BlockStore: IndexedBlockDagStorage](
      id: Int,
      b: BlockMessage,
      genesis: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): F[Unit] = {
    val updatedBlockPostState = b.getBody.getState.withPostStateHash(postGenStateHash)
    val updatedBlockBody =
      b.getBody.withState(updatedBlockPostState).withDeploys(processedDeploys)
    val updatedBlock = b.withBody(updatedBlockBody)
    BlockStore[F].put(b.blockHash, updatedBlock) *>
      IndexedBlockDagStorage[F].inject(id, updatedBlock, genesis, false)
  }
}

trait BlockGenerator {
  private def buildBlock[F[_]: Monad: Time](
      parentsHashList: Seq[BlockHash],
      creator: Validator,
      bonds: Seq[Bond],
      justifications: collection.Map[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy],
      tsHash: ByteString,
      shardId: String,
      preStateHash: ByteString,
      seqNum: Int
  ): F[BlockMessage] =
    for {
      now <- Time[F].currentMillis
      postState = RChainState()
        .withPreStateHash(preStateHash)
        .withPostStateHash(tsHash)
        .withBonds(bonds)
      postStateHash = Blake2b256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
        .withDeploysHash(ProtoUtil.protoSeqHash(deploys))
        .withTimestamp(now)
      blockHash = Blake2b256.hash(header.toByteArray)
      body      = Body().withState(postState).withDeploys(deploys)
      serializedJustifications = justifications.toList.map {
        case (cr: Validator, latestBlockHash: BlockHash) =>
          Justification(cr, latestBlockHash)
      }
      serializedBlockHash = ByteString.copyFrom(blockHash)
    } yield
      BlockMessage(
        serializedBlockHash,
        Some(header),
        Some(body),
        serializedJustifications,
        creator,
        shardId = shardId,
        seqNum = seqNum
      )

  def createGenesis[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): F[BlockMessage] =
    for {
      genesis <- buildBlock[F](
                  Seq.empty,
                  creator,
                  bonds,
                  justifications,
                  deploys,
                  tsHash,
                  shardId,
                  preStateHash,
                  seqNum
                )
      modifiedBlock <- IndexedBlockDagStorage[F].insertIndexed(genesis, genesis, false)
      _             <- BlockStore[F].put(genesis.blockHash, modifiedBlock)
    } yield modifiedBlock

  def createBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      parentsHashList: Seq[BlockHash],
      genesis: BlockMessage,
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[ProcessedDeploy] = Seq.empty[ProcessedDeploy],
      tsHash: ByteString = ByteString.EMPTY,
      shardId: String = "rchain",
      preStateHash: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): F[BlockMessage] =
    for {
      block <- buildBlock[F](
                parentsHashList,
                creator,
                bonds,
                justifications,
                deploys,
                tsHash,
                shardId,
                preStateHash,
                seqNum
              )
      modifiedBlock <- IndexedBlockDagStorage[F].insertIndexed(block, genesis, false)
      _             <- BlockStore[F].put(block.blockHash, modifiedBlock)
    } yield modifiedBlock
}
