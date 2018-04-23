package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.internals._
import coop.rchain.casper.protocol.{Resource => ResourceProto, _}
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.crypto.hash.Sha256

import scalaz._, Scalaz._

import scala.language.higherKinds

trait BlockGenerator {
  def chainState[F[_]: ChainState]: ChainState[F] = MonadState[F, Chain]
  def createBlock[F[_]: ChainState](parentsHashList: Seq[ByteString]): F[BlockMessage] =
    for {
      chain          <- chainState[F].get
      nextId         = chain.currentId + 1
      uniqueResource = ResourceProto(ProduceResource(Produce(nextId)))
      postState      = RChainState().withResources(Seq(uniqueResource))
      postStateHash  = Sha256.hash(postState.toByteArray)
      header = Header()
        .withPostStateHash(ByteString.copyFrom(postStateHash))
        .withParentsHashList(parentsHashList)
      blockHash                                     = Sha256.hash(header.toByteArray)
      body                                          = Body().withPostState(postState)
      block                                         = BlockMessage(ByteString.copyFrom(blockHash), Some(header), Some(body))
      idToBlocks: collection.Map[Int, BlockMessage] = chain.idToBlocks + (nextId -> block)
      hashToBlocks: collection.Map[ByteString, BlockMessage] = chain.hashToBlocks + (ByteString
        .copyFrom(blockHash) -> block)
      newChain: Chain = Chain(idToBlocks, hashToBlocks, nextId)
      _               <- chainState[F].put(newChain)
    } yield block
}
