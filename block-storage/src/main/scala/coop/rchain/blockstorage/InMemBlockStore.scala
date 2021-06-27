package coop.rchain.blockstorage

import cats.{Id, Monad}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage, BlockMessageProto}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash

import scala.language.higherKinds

class InMemBlockStore[F[_]: Monad: Metrics] private (
    refF: Ref[F, Map[BlockHash, BlockMessageProto]],
    approvedBlockRef: Ref[F, Option[ApprovedBlock]]
) extends BlockStore[F] {

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(BlockStorageMetricsSource, "in-mem")

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _     <- Metrics[F].incrementCounter("get")
      state <- refF.get
    } yield state.get(blockHash) >>= (bmp => BlockMessage.from(bmp).toOption)

  override def find(p: BlockHash => Boolean, n: Int): F[Seq[(BlockHash, BlockMessage)]] =
    for {
      _     <- Metrics[F].incrementCounter("find")
      state <- refF.get
    } yield state.filterKeys(p(_)).toSeq.map {
      case (bh, bmp) => (bh, BlockMessage.from(bmp).right.get) //TODO FIX-ME
    }

  def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- Metrics[F].incrementCounter("put")
      _ <- refF.update { state =>
            val (hash, message) = f
            state.updated(hash, BlockMessage.toProto(message))
          }
    } yield ()

  def getApprovedBlock: F[Option[ApprovedBlock]] =
    approvedBlockRef.get

  def putApprovedBlock(block: ApprovedBlock): F[Unit] =
    approvedBlockRef.set(Some(block))

  def checkpoint(): F[Unit] =
    ().pure[F]

  def clear(): F[Unit] =
    for {
      _ <- refF.update { _.empty }
    } yield ()

  override def close(): F[Unit] = ().pure[F]
}

object InMemBlockStore {
  def create[F[_]: Monad: Metrics](
      refF: Ref[F, Map[BlockHash, BlockMessageProto]],
      approvedBlockRef: Ref[F, Option[ApprovedBlock]]
  ): BlockStore[F] =
    new InMemBlockStore[F](refF, approvedBlockRef)

  def createWithId: BlockStore[Id] = {
    import coop.rchain.catscontrib.effect.implicits._
    import coop.rchain.metrics.Metrics.MetricsNOP
    val refId                = emptyMapRef[Id](syncId)
    val approvedBlockRef     = Ref[Id].of(none[ApprovedBlock])
    val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    InMemBlockStore.create(refId, approvedBlockRef)(syncId, metrics)
  }

  def emptyMapRef[F[_]](implicit syncEv: Sync[F]): F[Ref[F, Map[BlockHash, BlockMessageProto]]] =
    Ref[F].of(Map.empty[BlockHash, BlockMessageProto])

}
