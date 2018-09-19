package coop.rchain.rspace.concurrent

import java.util.concurrent.Semaphore

import scala.collection.concurrent.TrieMap

trait MultiLock[K] {
  def acquire[R](keys: Seq[K])(thunk: => R)(implicit o: Ordering[K]): R
}

class DefaultMultiLock[K] extends MultiLock[K] {

  import java.util.concurrent.Semaphore

  private[this] val locks = TrieMap.empty[K, Semaphore]

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

import cats._
import cats.implicits._
import cats.effect.Concurrent
import cats.effect.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class FunctionalMultiLock[F[_]: Concurrent, K] {

  import cats.effect.concurrent.Semaphore
  private[this] val locks = TrieMap.empty[K, Semaphore[F]]

  def acquire[R](keys: List[K])(thunk: => R)(implicit o: Ordering[K]): F[R] =
    (for {
      k <- keys.sorted.pure[F]
      semaphores <- (k.map { ks =>
                     Semaphore[F](1).map { l =>
                       locks.getOrElseUpdate(ks, l)
                     }
                   }).sequence
      openLocks <- (semaphores
                    .map(ss => {
                      ss.acquire.map { _ =>
                        ss
                      }
                    }))
                    .sequence
      res = thunk
      _   <- openLocks.map(_.release).sequence
    } yield (res))

}
