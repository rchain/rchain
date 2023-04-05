package coop.rchain.roscala.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantReadWriteLock

import coop.rchain.roscala.util.syntax._

class LockedMap[K, V] {
  protected val map = new ConcurrentHashMap[K, V]()

  val lock = new ReentrantReadWriteLock(false)

  def unsafeGet(i: K): V =
    lock.readLock().withLock {
      map.get(i)
    }

  def apply(i: K): Option[V] =
    lock.readLock().withLock {
      Option(map.get(i))
    }

  def update(i: K, v: V): Unit =
    lock.writeLock().withLock {
      map.put(i, v)
    }

  def useWithReadLock[R](f: AsyncHashMap[K, V] => R): R =
    lock.readLock().withLock {
      f(map)
    }

  def size: Int = map.size()
}
