package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger

import Ob.logger
import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.Ob.logger
import coop.rchain.roscala.ob.mbox.MboxOb
import coop.rchain.roscala.util.Slot

abstract class Ob {
  val slot       = Slot()
  var meta: Meta = _
  var parent: Ob = _

  def accepts(msg: Ctxt): Boolean = false

  def dispatch(ctxt: Ctxt, state: State): Ob = {
    logger.debug(s"Dispatch method not implemented for ${this.getClass.getSimpleName} class")
    Niv
  }

  // TODO: This method should be abstract
  def indexedSize(): Fixnum = Fixnum(0)

  def receiveMsg(client: MboxOb, task: Ctxt, state: State): Ob = Niv

  def nextMsg(client: MboxOb, newEnabledSet: Ob, state: State): Ob = Niv

  def extendWith(keyMeta: Ob, argvec: Tuple): Ob =
    if (keyMeta == NilMeta)
      this
    else
      argvec.becomeExtension(keyMeta.asInstanceOf[Meta], this)

  def invoke(ctxt: Ctxt, state: State): Ob = Niv

  /** Tries to lookup value for key and then invokes the value
    *
    * `ctxt.trgt` contains the key.
    */
  def lookupAndInvoke(ctxt: Ctxt, state: State): Ob = {
    val fn = meta.lookupObo(this, ctxt.trgt, state.globalEnv)
    logger.debug(s"Lookup and invoke $fn")
    fn.invoke(ctxt, state)
  }

  def lookup(key: Ob, globalEnv: GlobalEnv): Ob = {
    logger.debug(s"Lookup for $key in $this")
    val me     = this
    val result = meta.get(me, key, globalEnv)

    if (result == Absent)
      parent.lookup(key, globalEnv)
    else
      result
  }

  def update(enabledSetProvided: Boolean, ctxt: Ctxt, state: State): Ob = {
    val keyStart = if (enabledSetProvided) 1 else 0
    val me       = this
    val rslt     = this

    /**
      * `ctxt` has keys and values paired up.
      * For example the pair `Symbol(i) -> Fixnum(5)` could be stored
      * in `ctxt.argvec(0)` and `ctxt.argvec(1)`.
      *
      * If the `argvec` is ill-formed `Meta.set` will return an error
      * which `update` will forward.
      */
    if (ctxt.nargs > keyStart) {
      ctxt.argvec.value
        .drop(keyStart)
        .grouped(2)
        .toList
        .takeWhile { pair =>
          logger.debug(s"Set ${pair(0)} -> ${pair(1)} in $this")
          meta.set(this, pair(0), pair(1), ctxt, state.globalEnv) == me
        }
    }

    rslt
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

  override def toString: String =
    this.getClass.getSimpleName + "@" + Integer.toHexString(System.identityHashCode(this))

  def numberOfSlots: Int = slot.size

  def matches(ctxt: Ctxt): Boolean = false
}

object Ob {
  val logger = Logger("Ob")
}

case class Monitor() extends Ob
case class Symbol(value: String) extends Ob {
  override def toString: String =
    s"'$value"
}

case object Absent         extends Ob
case object Deadthread     extends Ob
case object Invalid        extends Ob
case object Niv            extends Ob
case object Qanon          extends Ob
case object Suicide        extends Ob
case object Suspended      extends Ob
case object Upcall         extends Ob
case object MissingBinding extends Ob
