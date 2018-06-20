package coop.rchain.roscala.ob

import coop.rchain.roscala.util.syntax._

class Extension extends Ob

class Actor extends Ob {
  val extension = new Extension()
  meta = Meta.empty

  /**
    * In Rosette `value` gets appended to the `extension` field and
    * `key` is ignored.
    */
  def addSlot(value: Ob): Int =
    extension.slot += value

}
