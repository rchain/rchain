package coop.rchain.rspace.concurrent

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
import scala.collection.immutable.Seq

import scala.collection.concurrent.TrieMap

class MultiLock[F[_]: Concurrent, K] {

  private[this] val locks = TrieMap.empty[K, Semaphore[F]]

  def acquire[R](keys: Seq[K])(thunk: => F[R])(implicit o: Ordering[K]): F[R] = {
    def acquireLocks =
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
    def releaseLocks(acquired: List[Semaphore[F]]) = acquired.traverse(_.release).as(())

    Concurrent[F].bracket(acquireLocks)(_ => thunk)(releaseLocks)
  }
}
