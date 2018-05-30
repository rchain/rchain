package coop.rchain.roscala.ob

import coop.rchain.roscala._
import coop.rchain.roscala.Location._

import scala.collection.mutable

/** Meta objects map keys to locations that point to values
  *
  * `refCount` holds the number of objects that share this meta entity.
  * `extensible` declares if key-value pairs can be added. Only `Actor`s and `TblObject`s are extensible.
  */
case class Meta(map: mutable.Map[Ob, Location], var refCount: Int, var extensible: Boolean)
    extends Ob {

  /** Add key-value pair for a given meta-client pair
    *
    * This creates a key-location pair in `this.map`. The location is a `LexVariable` that points to
    * a `slot` offset in `client` which then contains the `value` object.
    *
    * TODO: Check if `client` is extensible
    * TODO: Add `TblObject` case for `addSlot`
    * TODO: Add case where `meta` is shared
    */
  def add(client: Ob, key: Ob, value: Ob, ctxt: Ctxt)(globalEnv: GlobalEnv): Ob = {
    map.get(key) match {
      case Some(location) =>
        // `key` already exists
        setValWrt(location, client, value)(globalEnv)

      case None =>
        // key-value pair does not exist already

        // Add key-value pair to `client`
        val offset = client match {
          case actor: Actor => actor.addSlot(value)

          case _ =>
            suicide("Meta.add")
            0
        }

        // Add mapping of `key` to location that describes where `value` lives
        map(key) = LexVariable(level = 0, offset = offset, indirect = true)
    }

    client
  }

  /**
    * Get value for `key` in `client`
    */
  def get(client: Ob, key: Ob)(globalEnv: GlobalEnv): Ob = {
    var container = client

    map.get(key) match {
      case Some(location) =>
        location match {
          case LexVariable(_, offset, indirect) =>
            if (indirect) {
              container = client.asInstanceOf[Actor].extension
            }

            container.slot(offset)

          case _ => valWrt(location, client)(globalEnv)
        }

      case None => Absent
    }
  }

  def lookupObo(client: Ob, key: Ob)(globalEnv: GlobalEnv): Ob = {
    val result = get(client, key)(globalEnv)

    if (result == Absent)
      client.parent.lookup(key)(globalEnv)
    else
      result
  }

}

object Meta {
  def empty = Meta(map = mutable.Map(), 0, extensible = true)
}
