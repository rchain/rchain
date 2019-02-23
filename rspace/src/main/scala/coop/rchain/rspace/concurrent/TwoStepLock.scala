package coop.rchain.rspace.concurrent
import cats.Id
import cats.implicits._
import cats.effect.Concurrent
import scala.collection.immutable.Seq

trait TwoStepLock[F[_], K] {
  def acquire[R, S, W](keysA: Seq[K])(phaseTwo: () => F[Seq[K]])(thunk: => F[W])(
      implicit o: Ordering[K]
  ): F[W]
}

class ConcurrentTwoStepLockF[F[_]: Concurrent, K] extends TwoStepLock[F, K] {
  private[this] val phaseA: FunctionalMultiLock[F, K] = new FunctionalMultiLock[F, K]
  private[this] val phaseB: FunctionalMultiLock[F, K] = new FunctionalMultiLock[F, K]

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

class DefaultTwoStepLock[K] extends TwoStepLock[Id, K] {
  private[this] val phaseA: MultiLock[K] = new DefaultMultiLock[K]
  private[this] val phaseB: MultiLock[K] = new DefaultMultiLock[K]

  override def acquire[R, S, W](
      keysA: Seq[K]
  )(phaseTwo: () => Seq[K])(thunk: => W)(implicit o: Ordering[K]): W =
    phaseA.acquire(keysA) {
      val keysB = phaseTwo
      phaseB.acquire(keysB()) {
        thunk
      }
    }
}
