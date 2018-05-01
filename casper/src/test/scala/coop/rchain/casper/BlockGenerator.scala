package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.protocol.{Resource => ResourceProto, _}
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.crypto.hash.Sha256

import scala.collection.immutable.{HashMap, HashSet}
import scala.language.higherKinds
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.casper.Estimator.{BlockHash, Validator}

trait BlockGenerator {
  def createBlock[F[_]: Monad: BlockDagState](parentsHashList: Seq[BlockHash]): F[BlockMessage] =
    createBlock[F](parentsHashList, ByteString.EMPTY)
  def createBlock[F[_]: Monad: BlockDagState](parentsHashList: Seq[BlockHash],
                                              creator: Validator): F[BlockMessage] =
    createBlock[F](parentsHashList, creator, Seq.empty[Bond])
  def createBlock[F[_]: Monad: BlockDagState](parentsHashList: Seq[BlockHash],
                                              creator: Validator,
                                              bonds: Seq[Bond]): F[BlockMessage] =
    createBlock[F](parentsHashList, creator, bonds, HashMap.empty[Validator, BlockHash])
  def createBlock[F[_]: Monad: BlockDagState](
      parentsHashList: Seq[BlockHash],
      creator: Validator,
      bonds: Seq[Bond],
      justifications: collection.Map[Validator, BlockHash]): F[BlockMessage] =
    for {
      chain          <- blockDagState[F].get
      nextId         = chain.currentId + 1
      uniqueResource = ResourceProto(ProduceResource(Produce(nextId)))
      postState      = RChainState().withResources(Seq(uniqueResource)).withBonds(bonds)
      postStateHash  = Sha256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
      blockHash = Sha256.hash(header.toByteArray)
      body      = Body().withPostState(postState)
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
      childMap: HashMap[BlockHash, HashSet[BlockHash]] = chain.childMap ++ updatedChildren
      newChain: BlockDag                               = BlockDag(idToBlocks, blockLookup, childMap, chain.latestMessages, nextId)
      _                                                <- blockDagState[F].set(newChain)
    } yield block
}
