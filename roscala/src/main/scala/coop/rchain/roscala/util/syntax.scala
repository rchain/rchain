package coop.rchain.roscala.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.Lock

object syntax {
  implicit class LockOps(val lock: Lock) extends AnyVal {
    @inline def withLock[A](f: => A): A = {
      lock.lock()

      try {
        f
      } finally {
        lock.unlock()
      }
    }
  }

  implicit class ConcurrentHashMapOps[K, V](val hm: AsyncHashMap[K, V]) extends AnyVal {
    @inline def apply(k: K): V = hm.get(k)

    @inline def update(k: K, v: V) = hm.put(k, v)
  }
}
