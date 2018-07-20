package coop.rchain.blockstorage

import cats._
import cats.effect.concurrent.Ref
import cats.effect.{ExitCase, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics
import coop.rchain.shared.SyncVarOps

import scala.collection.mutable.ListBuffer
import scala.concurrent.SyncVar
import scala.language.higherKinds

class InMemBlockStore[F[_], E] private ()(implicit
                                          monadF: Monad[F],
                                          refF: Ref[F, Map[BlockHash, BlockMessage]],
                                          metricsF: Metrics[F])
    extends BlockStore[F] {

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _     <- metricsF.incrementCounter("block-store-get")
      state <- refF.get
    } yield state.get(blockHash)

  @deprecated(message = "to be removed when casper code no longer needs the whole DB in memmory",
              since = "0.5")
  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _     <- metricsF.incrementCounter("block-store-as-map")
      state <- refF.get
    } yield state

  def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- metricsF.incrementCounter("block-store-put")
      _ <- refF.update { state =>
            val (hash, message) = f
            state.updated(hash, message)
          }
    } yield ()
}

object InMemBlockStore {
  def create[F[_], E](implicit
                      monadF: Monad[F],
                      refF: Ref[F, Map[BlockHash, BlockMessage]],
                      metricsF: Metrics[F]): BlockStore[F] =
    new InMemBlockStore()

  def createWithId: BlockStore[Id] = {
    import coop.rchain.metrics.Metrics.MetricsNOP
    implicit val implicitSyncId       = syncId
    val refId                         = Ref[Id].of(Map.empty[BlockHash, BlockMessage])
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    InMemBlockStore.create(syncId, refId, metrics)
  }

  val syncId: Sync[Id] = new Sync[Id] {
    override def suspend[A](thunk: => Id[A]): Id[A] = thunk

    override def bracketCase[A, B](acquire: Id[A])(use: A => Id[B])(
        release: (A, ExitCase[Throwable]) => Id[Unit]): Id[B] = ???

    override def raiseError[A](e: Throwable): Id[A] = ???

    override def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] = ???

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???

    override def pure[A](x: A): Id[A] = x
  }
}
