package coop.rchain.roscala.ob

class Extension extends Ob

class Actor extends Ob {
  val extension = new Extension()
  meta = Meta.empty

  /**
    * In Rosette `value` gets appended to the `extension` field and
    * `key` is ignored.
    */
  def addSlot(value: Ob): Int = {
    val n = extension.slot.size
    extension.slot += value
    n
  }

}
