package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import Ob.logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import java.util.concurrent.ConcurrentHashMap

import coop.rchain.roscala.util.{LockedMap, Slot}

abstract class Ob {
  val slot       = Slot()
  var meta: Meta = _
  var parent: Ob = _

  def dispatch(state: State, globalEnv: GlobalEnv): Ob = Niv

  def extendWith(keyMeta: Ob, argvec: Tuple): Ob =
    if (keyMeta == NilMeta)
      this
    else
      argvec.becomeExtension(keyMeta.asInstanceOf[Meta], this)

  def invoke(state: State, globalEnv: GlobalEnv): Ob = Niv

  /** Tries to lookup value for key and then invokes the value
    *
    * `ctxt.trgt` contains the key.
    */
  def lookupAndInvoke(state: State, globalEnv: GlobalEnv): Ob = {
    val fn = meta.lookupObo(this, state.ctxt.trgt)(globalEnv)
    logger.debug(s"Lookup and invoke $fn")
    fn.invoke(state, globalEnv)
  }

  def lookup(key: Ob)(globalEnv: GlobalEnv): Ob = {
    logger.debug(s"Lookup for $key in $this")
    val me     = this
    val result = meta.get(me, key)(globalEnv)

    if (result == Absent)
      parent.lookup(key)(globalEnv)
    else
      result
  }

  def getLex(indirect: Boolean, level: Int, offset: Int): Ob = {
    var p = this

    for (_ <- 0 until level) p = p.parent

    if (indirect) {
      p = p.asInstanceOf[Actor].extension
    }

    p.slot(offset).getOrElse(Invalid)
  }

  def setLex(indirect: Boolean, level: Int, offset: Int, value: Ob): Ob = {
    var p = this

    for (_ <- 0 until level) p = p.parent

    if (indirect) {
      p = p.asInstanceOf[Actor].extension
    }

    p.slot(offset) = value
    value
  }

  def numberOfSlots = slot.size
}

object Ob {
  val logger = Logger("Ob")
}

case class Monitor()             extends Ob
case class Symbol(value: String) extends Ob

case object Absent     extends Ob
case object Suicide    extends Ob
case object Invalid    extends Ob
case object Upcall     extends Ob
case object Deadthread extends Ob
case object Niv        extends Ob
case object RblFalse   extends Ob
case object RblTrue    extends Ob
