package coop.rchain.roscala.ob

import coop.rchain.roscala.util.Slot
import coop.rchain.roscala.util.syntax._

class TblObject extends Actor {
  val keyVec = Slot()

  def entry(n: Int): Ob = extension.slot.unsafeGet(n)

  def entryKey(n: Int): Ob = keyVec.unsafeGet(n)

  def addSlot(key: Ob, value: Ob): Int =
    //necessary because we want to update `keyVec` and `extension.slot` atomically
    //otherwise another thread could try to access an uninitialized associated value
    keyVec.lock.writeLock().withLock {
      extension.slot.lock.writeLock().withLock {
        keyVec += key
        super.addSlot(value)
      }
    }
}
