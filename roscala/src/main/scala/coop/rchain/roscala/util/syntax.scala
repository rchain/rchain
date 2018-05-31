package coop.rchain.roscala.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.Lock

object syntax {
  implicit class LockOps(lock: Lock) {
    def withLock[A](f: => A): A = {
      lock.lock()

      try {
        f
      } finally {
        lock.unlock()
      }
    }
  }

  implicit class ConcurrentHashMapOps[K, V](hm: ConcurrentHashMap[K, V]) {
    def apply(k: K): V = hm.get(k)

    def update(k: K, v: V) = hm.put(k, v)
  }
}
