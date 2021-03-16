package coop.rchain.rspace.concurrent

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.metrics.Metrics

trait TwoStepLock[F[_], K] {
  def acquire[R, S, W](keysA: Seq[K])(phaseTwo: () => F[Seq[K]])(thunk: => F[W])(
      implicit o: Ordering[K]
  ): F[W]
}

class ConcurrentTwoStepLockF[F[_]: Concurrent: Metrics, K](ms: Metrics.Source)
    extends TwoStepLock[F, K] {
  private[this] val phaseA: MultiLock[F, K] =
    new MultiLock[F, K](Metrics.Source(ms, "two-step-lock.phase-a"))
  private[this] val phaseB: MultiLock[F, K] =
    new MultiLock[F, K](Metrics.Source(ms, "two-step-lock.phase-b"))

  override def acquire[R, S, W](keysA: Seq[K])(phaseTwo: () => F[Seq[K]])(thunk: => F[W])(
      implicit o: Ordering[K]
  ): F[W] =
    phaseA.acquire(keysA) {
      for {
        keysB <- phaseTwo()
        res   <- phaseB.acquire(keysB)(thunk)
      } yield res
    }

  def cleanUp: F[Unit] =
    for {
      _ <- phaseA.cleanUp
      _ <- phaseB.cleanUp
    } yield ()
}
