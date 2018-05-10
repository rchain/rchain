package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.hash.Sha256

import scala.collection.immutable.{HashMap, HashSet}
import scala.language.higherKinds
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._

trait BlockGenerator {

  def createBlock[F[_]: Monad: BlockDagState](
      parentsHashList: Seq[BlockHash],
      creator: Validator = ByteString.EMPTY,
      bonds: Seq[Bond] = Seq.empty[Bond],
      justifications: collection.Map[Validator, BlockHash] = HashMap.empty[Validator, BlockHash],
      deploys: Seq[Deploy] = Seq.empty[Deploy]): F[BlockMessage] =
    for {
      chain         <- blockDagState[F].get
      nextId        = chain.currentId + 1
      postState     = RChainState().withBonds(bonds).withBlockNumber(nextId.toLong)
      postStateHash = Sha256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
      blockHash = Sha256.hash(header.toByteArray)
      body      = Body().withPostState(postState).withNewCode(deploys)
      serializedJustifications = justifications.toList.map {
        case (creator: Validator, latestBlockHash: BlockHash) =>
          Justification(creator, latestBlockHash)
      }
      serializedBlockHash = ByteString.copyFrom(blockHash)
      block = BlockMessage(serializedBlockHash,
                           Some(header),
                           Some(body),
                           serializedJustifications,
                           creator)
      idToBlocks  = chain.idToBlocks + (nextId               -> block)
      blockLookup = chain.blockLookup + (serializedBlockHash -> block)
      updatedChildren = HashMap[BlockHash, HashSet[BlockHash]](parentsHashList.map {
        parentHash: BlockHash =>
          val currentChildrenHashes = chain.childMap.getOrElse(parentHash, HashSet.empty[BlockHash])
          val updatedChildrenHashes = currentChildrenHashes + serializedBlockHash
          parentHash -> updatedChildrenHashes
      }: _*)
      childMap = chain.childMap
        .++[(BlockHash, HashSet[BlockHash]), HashMap[BlockHash, HashSet[BlockHash]]](
          updatedChildren)
      newChain: BlockDag = BlockDag(idToBlocks, blockLookup, childMap, chain.latestMessages, nextId)
      _                  <- blockDagState[F].set(newChain)
    } yield block
}
