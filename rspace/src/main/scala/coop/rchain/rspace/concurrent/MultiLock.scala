package coop.rchain.rspace.concurrent

import scala.collection.concurrent.TrieMap

import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
import cats.implicits._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.Source

class MultiLock[F[_]: Concurrent: Metrics, K](metricSource: Metrics.Source) {

  implicit private val ms: Source = metricSource
  private[this] val locks         = TrieMap.empty[K, Semaphore[F]]

  def acquire[R](
      keys: Seq[K]
  )(thunk: => F[R])(implicit o: Ordering[K]): F[R] = {
    def acquireLocks: F[List[Semaphore[F]]] =
      for {
        semaphores <- keys.toSet.toList.sorted.traverse(
                       k =>
                         Semaphore[F](1)
                           .map { s => // if the lock exists this is waste. how to avoid?
                             locks.getOrElseUpdate(k, s)
                           }
                     )
        acquired <- semaphores.traverse(s => s.acquire.map(_ => s))
      } yield acquired

    def releaseLocks(acquired: List[Semaphore[F]]): F[Unit] = acquired.traverse(_.release).as(())

    import coop.rchain.metrics.implicits._

    Concurrent[F].bracket(
      for {
        _     <- Metrics[F].incrementGauge("lock.queue")
        locks <- Concurrent[F].defer(acquireLocks).timer("lock.acquire")
        _     <- Metrics[F].decrementGauge("lock.queue")
      } yield locks
    )(kp(thunk))(releaseLocks)
  }
}
