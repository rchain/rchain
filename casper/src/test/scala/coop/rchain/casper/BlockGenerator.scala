package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.internals._
import coop.rchain.casper.protocol.{Resource => ResourceProto, _}
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.crypto.hash.Sha256

import scala.collection.immutable.HashMap
import scala.language.higherKinds

import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._

trait BlockGenerator {
  def createBlock[F[_]: Monad: ChainState](parentsHashList: Seq[ByteString]): F[BlockMessage] =
    createBlock[F](parentsHashList, ByteString.EMPTY)
  def createBlock[F[_]: Monad: ChainState](parentsHashList: Seq[ByteString],
                                           creator: ByteString): F[BlockMessage] =
    createBlock[F](parentsHashList, creator, Seq.empty[Bond])
  def createBlock[F[_]: Monad: ChainState](parentsHashList: Seq[ByteString],
                                           creator: ByteString,
                                           bonds: Seq[Bond]): F[BlockMessage] =
    createBlock[F](parentsHashList, creator, bonds, HashMap.empty[ByteString, ByteString])
  def createBlock[F[_]: Monad: ChainState](
      parentsHashList: Seq[ByteString],
      creator: ByteString,
      bonds: Seq[Bond],
      justifications: collection.Map[ByteString, ByteString]): F[BlockMessage] =
    for {
      chain          <- chainState[F].get
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
        case (creator: ByteString, latestBlockHash: ByteString) =>
          Justification(creator, latestBlockHash)
      }
      block = BlockMessage(ByteString.copyFrom(blockHash),
                           Some(header),
                           Some(body),
                           serializedJustifications,
                           creator)
      idToBlocks: collection.Map[Int, BlockMessage] = chain.idToBlocks + (nextId -> block)
      hashToBlocks: collection.Map[ByteString, BlockMessage] = chain.hashToBlocks + (ByteString
        .copyFrom(blockHash) -> block)
      newChain: Chain = Chain(idToBlocks, hashToBlocks, nextId)
      _               <- chainState[F].set(newChain)
    } yield block
}
