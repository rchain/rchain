package coop.rchain.blockstorage

import cats.{Id, Monad}
import cats.implicits._
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.hash.Blake2b256

final class IndexedBlockDagStorage[F[_]: Monad](
    lock: Semaphore[F],
    underlying: BlockDagStorage[F],
    idToBlocksRef: Ref[F, Map[Int, BlockMessage]],
    currentIdRef: Ref[F, Int]
) extends BlockDagStorage[F] {
  def getRepresentation: F[BlockDagRepresentation[F]] =
    for {
      _      <- lock.acquire
      result <- underlying.getRepresentation
      _      <- lock.release
    } yield result
  def insert(block: BlockMessage): F[Unit] =
    for {
      _ <- lock.acquire
      _ <- underlying.insert(block)
      _ <- lock.release
    } yield ()
  def insertIndexed(block: BlockMessage): F[BlockMessage] =
    for {
      _                 <- lock.acquire
      body              = block.body.get
      header            = block.header.get
      currentId         <- currentIdRef.get
      nextId            = currentId + 1
      dag               <- underlying.getRepresentation
      nextCreatorSeqNum <- dag.latestMessage(block.sender).map(_.fold(-1)(_.seqNum) + 1)
      newPostState      = body.postState.get.withBlockNumber(nextId)
      newPostStateHash  = Blake2b256.hash(newPostState.toByteArray)
      modifiedBlock = block
        .withBody(body.withPostState(newPostState))
        .withHeader(header.withPostStateHash(ByteString.copyFrom(newPostStateHash)))
        .withSeqNum(nextCreatorSeqNum)
      _ <- underlying.insert(modifiedBlock)
      _ <- idToBlocksRef.update(_.updated(nextId, modifiedBlock))
      _ <- currentIdRef.set(nextId)
      _ <- lock.release
    } yield modifiedBlock
  def checkpoint(): F[Unit] = underlying.checkpoint()
  def clear(): F[Unit] =
    for {
      _ <- lock.acquire
      _ <- underlying.clear()
      _ <- idToBlocksRef.set(Map.empty)
      _ <- currentIdRef.set(-1)
      _ <- lock.release
    } yield ()
  def close(): F[Unit] = underlying.close()
  def lookupById(id: Int): F[Option[BlockMessage]] =
    for {
      idToBlocks <- idToBlocksRef.get
    } yield idToBlocks.get(id)
  def lookupByIdUnsafe(id: Int): F[BlockMessage] =
    for {
      idToBlocks <- idToBlocksRef.get
    } yield idToBlocks(id)
}

object IndexedBlockDagStorage {
  def apply[F[_]](implicit B: IndexedBlockDagStorage[F]): IndexedBlockDagStorage[F] = B

  def createWithId(underlying: BlockDagStorage[Id]): IndexedBlockDagStorage[Id] = {
    import coop.rchain.catscontrib.effect.implicits._
    new IndexedBlockDagStorage[Id](
      Semaphore[Id](1),
      underlying,
      Ref.of[Id, Map[Int, BlockMessage]](Map.empty),
      Ref.of[Id, Int](-1)
    )
  }
}
