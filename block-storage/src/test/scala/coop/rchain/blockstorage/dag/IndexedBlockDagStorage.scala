package coop.rchain.blockstorage.dag

import cats.Monad
import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStorageMetricsSource
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}

final class IndexedBlockDagStorage[F[_]: Monad](
    lock: Semaphore[F],
    underlying: BlockDagStorage[F],
    idToBlocksRef: Ref[F, Map[Long, BlockMessage]],
    currentIdRef: Ref[F, Long]
) extends BlockDagStorage[F] {

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(
      underlying.getRepresentation
    )

  def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    lock.withPermit(
      underlying.insert(block, genesis, invalid) >> underlying.getRepresentation
    )

  def insertIndexed(block: BlockMessage, genesis: BlockMessage, invalid: Boolean): F[BlockMessage] =
    lock.withPermit(for {
      currentId         <- currentIdRef.get
      dag               <- underlying.getRepresentation
      nextCreatorSeqNum <- dag.latestMessage(block.sender).map(_.fold(-1)(_.seqNum) + 1)
      nextId            = if (block.seqNum == 0) currentId + 1L else block.seqNum.toLong
      newPostState      = block.body.state.copy(blockNumber = nextId)
      newPostStateHash  = Blake2b256.hash(newPostState.toProto.toByteArray)
      modifiedBlock = block
        .copy(
          body = block.body.copy(state = newPostState),
          seqNum = nextCreatorSeqNum
        )
      _ <- underlying.insert(modifiedBlock, genesis, invalid)
      _ <- idToBlocksRef.update(_.updated(nextId, modifiedBlock))
      _ <- currentIdRef.set(nextId)
    } yield modifiedBlock)

  def inject(index: Int, block: BlockMessage, genesis: BlockMessage, invalid: Boolean): F[Unit] =
    lock
      .withPermit(
        idToBlocksRef
          .update(_.updated(index.toLong, block)) >> underlying.insert(block, genesis, invalid)
      )
      .void

  def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(
      underlying.accessEquivocationsTracker(f)
    )

  def checkpoint(): F[Unit] = underlying.checkpoint()

  def close(): F[Unit] = underlying.close()

  def lookupById(id: Int): F[Option[BlockMessage]] =
    idToBlocksRef.get.map(_.get(id.toLong))

  def lookupByIdUnsafe(id: Int): F[BlockMessage] =
    idToBlocksRef.get.map(_(id.toLong))
}

object IndexedBlockDagStorage {
  implicit private val IndexedBlockDagStorageMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-indexed")

  def apply[F[_]](implicit B: IndexedBlockDagStorage[F]): IndexedBlockDagStorage[F] = B

  def create[F[_]: Concurrent: Metrics](
      underlying: BlockDagStorage[F]
  ): F[IndexedBlockDagStorage[F]] =
    for {
      semaphore  <- MetricsSemaphore.single[F]
      idToBlocks <- Ref.of[F, Map[Long, BlockMessage]](Map.empty)
      currentId  <- Ref.of[F, Long](-1L)
    } yield new IndexedBlockDagStorage[F](
      semaphore,
      underlying,
      idToBlocks,
      currentId
    )
}
