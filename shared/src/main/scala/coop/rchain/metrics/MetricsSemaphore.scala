package coop.rchain.metrics

import cats.effect._
import cats.syntax.all._
import coop.rchain.catscontrib.ski.kp
import cats.effect.std.Semaphore
import cats.~>
import coop.rchain.metrics.implicits.MetricsSyntaxConversion

class MetricsSemaphore[F[_]: Sync: Metrics](
    underlying: Semaphore[F]
)(implicit ms: Metrics.Source)
    extends Semaphore[F] {
  import coop.rchain.metrics.implicits._

  def available: F[Long] = underlying.available
  def count: F[Long]     = underlying.count
  def acquireN(n: Long): F[Unit] =
    for {
      _ <- Metrics[F].incrementGauge("lock.queue")
      _ <- Sync[F].defer(underlying.acquireN(n)).timer("lock.acquire")
      _ <- Metrics[F].decrementGauge("lock.queue")
    } yield ()

  def tryAcquireN(n: Long): F[Boolean] = underlying.tryAcquireN(n)
  def releaseN(n: Long): F[Unit]       = underlying.releaseN(n)
  def withPermit[A](t: F[A]): F[A]     = permit.use(_ => t)

  override def permit: Resource[F, Unit] =
    Resource.make(Metrics[F].incrementGauge("lock.permit"))(
      _ => Metrics[F].decrementGauge("lock.permit")
    )

  override def mapK[G[_]](f: F ~> G)(implicit G: MonadCancel[G, _]): Semaphore[G] = ???
}

object MetricsSemaphore {

  def apply[F[_]: Sync: Metrics](
      underlying: Semaphore[F]
  )(implicit ms: Metrics.Source): MetricsSemaphore[F] =
    new MetricsSemaphore(underlying)

  def apply[F[_]: Async: Metrics](
      n: Long
  )(implicit ms: Metrics.Source): F[MetricsSemaphore[F]] =
    Semaphore[F](n).map(apply(_))

  def single[F[_]: Async: Metrics](implicit ms: Metrics.Source): F[MetricsSemaphore[F]] =
    apply(1L)
}
