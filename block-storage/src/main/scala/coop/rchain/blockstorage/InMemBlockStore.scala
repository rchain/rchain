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
    val refId                         = emptyMapRef[Id](syncId)
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    InMemBlockStore.create(syncId, refId, metrics)
  }

  def emptyMapRef[F[_]](implicit syncEv: Sync[F]): F[Ref[F, Map[BlockHash, BlockMessage]]] =
    Ref[F].of(Map.empty[BlockHash, BlockMessage])

  val syncId: Sync[Id] = new Sync[Id] {
    def pure[A](x: A): cats.Id[A] = x

    def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] = ???

    def raiseError[A](e: Throwable): cats.Id[A] = ???

    def bracketCase[A, B](acquire: cats.Id[A])(use: A => cats.Id[B])(
        release: (A, cats.effect.ExitCase[Throwable]) => cats.Id[Unit]): cats.Id[B] = ???

    def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
      implicitly[Monad[Id]].flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] = ???

    def suspend[A](thunk: => cats.Id[A]): cats.Id[A] = thunk
  }
}
