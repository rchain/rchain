package coop.rchain.roscala.ob

import scala.collection.mutable

class TblObject extends Actor {
  val keyVec = mutable.ArrayBuffer[Ob]()

  def addSlot(key: Ob, value: Ob): Int = {
    keyVec.append(key)
    super.addSlot(value)
  }
}
