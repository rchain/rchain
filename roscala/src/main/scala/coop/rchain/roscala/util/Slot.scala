package coop.rchain.roscala.util

import coop.rchain.roscala.ob.Ob
import coop.rchain.roscala.util.syntax._

class Slot extends LockedMap[Int, Ob] {

  def +=(v: Ob): Int =
    lock.writeLock().withLock {
      val size = map.size()
      map.put(size, v)
      size
    }
}

object Slot {
  def apply(): Slot = new Slot
}
