package coop.rchain.blockstorage

import cats.effect.Concurrent
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
      newPostState      = body.getState.withBlockNumber(nextId)
      newPostStateHash  = Blake2b256.hash(newPostState.toByteArray)
      modifiedBlock = block
        .withBody(body.withState(newPostState))
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

  def create[F[_]: Concurrent](underlying: BlockDagStorage[F]): F[IndexedBlockDagStorage[F]] =
    for {
      semaphore  <- Semaphore[F](1)
      idToBlocks <- Ref.of[F, Map[Int, BlockMessage]](Map.empty)
      currentId  <- Ref.of[F, Int](-1)
    } yield
      new IndexedBlockDagStorage[F](
        semaphore,
        underlying,
        idToBlocks,
        currentId
      )
}
