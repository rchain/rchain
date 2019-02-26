package coop.rchain.rspace.concurrent

import cats.implicits._
import cats.effect.Concurrent
import scala.collection.immutable.Seq

trait TwoStepLock[F[_], K] {
  def acquire[R, S, W](keysA: Seq[K])(phaseTwo: () => F[Seq[K]])(thunk: => F[W])(
      implicit o: Ordering[K]
  ): F[W]
}

class ConcurrentTwoStepLockF[F[_]: Concurrent, K] extends TwoStepLock[F, K] {
  private[this] val phaseA: MultiLock[F, K] = new MultiLock[F, K]
  private[this] val phaseB: MultiLock[F, K] = new MultiLock[F, K]

  override def acquire[R, S, W](keysA: Seq[K])(phaseTwo: () => F[Seq[K]])(thunk: => F[W])(
      implicit o: Ordering[K]
  ): F[W] =
    phaseA.acquire(keysA) {
      for {
        keysB <- phaseTwo()
        res   <- phaseB.acquire(keysB)(thunk)
      } yield res
    }
}
