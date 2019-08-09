package coop.rchain.blockstorage

import cats.effect.Sync
import cats.implicits._
import com.google.common.cache.{Cache, CacheBuilder}
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

class CachingBlockStore[F[_]: Sync: Metrics] private (
    underlying: BlockStore[F],
    cache: Cache[BlockHash, BlockMessage]
) extends BlockStore[F] {
  implicit private val ms = Metrics.Source(BlockStorageMetricsSource, "caching-block-storage")

  override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    Sync[F].delay(Option(cache.getIfPresent(blockHash))) >>= {
      case Some(blockMessage) =>
        Metrics[F].incrementCounter("get-hit") >> blockMessage.some.pure[F]
      case None =>
        for {
          _               <- Metrics[F].incrementCounter("get-miss")
          blockMessageOpt <- underlying.get(blockHash)
          _               <- blockMessageOpt.traverse_(b => Sync[F].delay(cache.put(blockHash, b)))
        } yield blockMessageOpt
    }

  override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
    underlying.find(p)

  override def put(f: => (BlockHash, BlockMessage)): F[Unit] = {
    val (blockHash, blockMessage) = f
    Sync[F].delay(cache.put(blockHash, blockMessage)) >> underlying.put(f)
  }

  override def getApprovedBlock: F[Option[ApprovedBlock]] =
    underlying.getApprovedBlock

  override def putApprovedBlock(block: ApprovedBlock): F[Unit] =
    underlying.putApprovedBlock(block)

  override def checkpoint(): F[Unit] =
    underlying.checkpoint()

  override def clear(): F[Unit] =
    Sync[F].delay(cache.invalidateAll()) >> underlying.clear()

  override def close(): F[Unit] =
    underlying.close()
}

object CachingBlockStore {
  def apply[F[_]: Sync: Metrics](
      underlying: BlockStore[F],
      maxSize: Long
  ): F[BlockStore[F]] =
    for {
      cache <- Sync[F].delay {
                CacheBuilder
                  .newBuilder()
                  .maximumWeight(maxSize)
                  .weigher[BlockHash, BlockMessage]((_, value) => value.serializedSize)
                  .build[BlockHash, BlockMessage]()
              }
    } yield new CachingBlockStore(underlying, cache)
}
