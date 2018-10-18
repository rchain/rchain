package coop.rchain.blockstorage

import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics

import scala.language.higherKinds

class InMemBlockStore[F[_]] private ()(
    implicit
    monadF: Monad[F],
    refF: Ref[F, Map[BlockHash, BlockMessage]],
    metricsF: Metrics[F]
) extends BlockStore[F] {

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _     <- metricsF.incrementCounter("block-store-get")
      state <- refF.get
    } yield state.get(blockHash)

  @deprecated(
    message = "to be removed when casper code no longer needs the whole DB in memmory",
    since = "0.5"
  )
  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _     <- metricsF.incrementCounter("block-store-as-map")
      state <- refF.get
    } yield state

  override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
    for {
      _     <- metricsF.incrementCounter("block-store-find")
      state <- refF.get
    } yield state.filterKeys(p(_)).toSeq

  def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- metricsF.incrementCounter("block-store-put")
      _ <- refF.update { state =>
            val (hash, message) = f
            state.updated(hash, message)
          }
    } yield ()

  def clear(): F[Unit] =
    for {
      _ <- refF.update { _.empty }
    } yield ()

  override def close(): F[Unit] = monadF.pure(())
}

object InMemBlockStore {
  def create[F[_]](
      implicit
      monadF: Monad[F],
      refF: Ref[F, Map[BlockHash, BlockMessage]],
      metricsF: Metrics[F]
  ): BlockStore[F] =
    new InMemBlockStore()

  def createWithId: BlockStore[Id] = {
    import coop.rchain.catscontrib.effect.implicits._
    import coop.rchain.metrics.Metrics.MetricsNOP
    val refId                         = emptyMapRef[Id](syncId)
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    InMemBlockStore.create(syncId, refId, metrics)
  }

  def emptyMapRef[F[_]](implicit syncEv: Sync[F]): F[Ref[F, Map[BlockHash, BlockMessage]]] =
    Ref[F].of(Map.empty[BlockHash, BlockMessage])

}
