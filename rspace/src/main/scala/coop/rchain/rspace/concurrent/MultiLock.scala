package coop.rchain.rspace.concurrent

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.catscontrib.ski.kp
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.Source

import scala.collection.concurrent.TrieMap
import cats.effect.std.Semaphore

class MultiLock[F[_]: Concurrent: Metrics, K](metricSource: Metrics.Source) {

  implicit private val ms: Source = metricSource

  private[this] val locks = TrieMap.empty[K, Semaphore[F]]

  def acquire[R](
      keys: Seq[K]
  )(thunk: => F[R])(implicit o: Ordering[K]): F[R] = {
    def acquireLocks: F[List[Semaphore[F]]] =
      for {
        semaphores <- keys.toSet.toList.sorted.traverse(
                       k =>
                         Semaphore[F](1)
                           .map { s =>
                             locks.getOrElseUpdate(k, s)
                           }
                     )
        acquired <- semaphores.traverse(s => s.acquire.as(s))
      } yield acquired

    def releaseLocks(acquired: List[Semaphore[F]]): F[Unit] = acquired.traverse_(_.release)

    import coop.rchain.metrics.implicits._

    Concurrent[F].bracket(
      for {
        _     <- Metrics[F].incrementGauge("lock.queue")
        locks <- acquireLocks.timer("lock.acquire")
        _     <- Metrics[F].decrementGauge("lock.queue")
      } yield locks
    )(kp(thunk))(releaseLocks)
  }

  // Release memory (stored semaphores)
  def cleanUp: F[Unit] = Sync[F].delay(locks.clear)
}
