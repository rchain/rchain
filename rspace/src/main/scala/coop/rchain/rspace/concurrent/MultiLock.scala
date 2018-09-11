package coop.rchain.rspace.concurrent

import scala.collection.concurrent.TrieMap

trait MultiLock[K] {

  def acquire[R](keys: Seq[K])(thunk: => R)(implicit o: Ordering[K]): R

}

class DefaultMultiLock[K] extends MultiLock[K] {

  import java.util.concurrent.Semaphore

  private[this] val locks = TrieMap.empty[K, Semaphore]

  def acquire[R](keys: Seq[K])(thunk: => R)(implicit o: Ordering[K]): R = {
    for {
      k         <- keys.sorted
      semaphore = locks.getOrElseUpdate(k, new Semaphore(1))
      _         = semaphore.acquire()
    } yield (())
    val r = thunk

    for {
      k         <- keys.sorted
      semaphore = locks.get(k).getOrElse(throw new Exception("This cannot happen"))
      _         = semaphore.release()
    } yield (())
    r
  }

}
