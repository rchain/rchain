package coop.rchain.rspace.concurrent

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
import scala.collection.immutable.Seq

import scala.collection.concurrent.TrieMap

trait MultiLock[K] {
  def acquire[R](keys: Seq[K])(thunk: => R)(implicit o: Ordering[K]): R
}

class DefaultMultiLock[K] extends MultiLock[K] {

  import java.util.concurrent.Semaphore

  private[this] val locks = TrieMap.empty[K, Semaphore]

  @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements")) // TODO stop throwing exceptions
  def acquire[R](keys: Seq[K])(thunk: => R)(implicit o: Ordering[K]): R = {
    // TODO: keys should be a Set[K]
    val sortedKeys = keys.toSet.toList.sorted
    for {
      k         <- sortedKeys
      semaphore = locks.getOrElseUpdate(k, new Semaphore(1))
      _         = semaphore.acquire()
    } yield ()
    try {
      thunk
    } finally {
      for {
        k         <- sortedKeys
        semaphore = locks.getOrElse(k, throw new Exception(s"Couldn't find lock for channel $k"))
        _         = semaphore.release()
      } yield ()
    }
  }

}

class FunctionalMultiLock[F[_]: Concurrent, K] {

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
