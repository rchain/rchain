package coop.rchain.roscala.ob

import scala.collection.mutable

abstract class Ob {
  val slot: mutable.ArrayBuffer[Ob] = mutable.ArrayBuffer[Ob]()
  var meta: Meta                    = _
  var parent: Ob                    = _

  def dispatch(ctxt: Ctxt): Ob = Niv

  def invoke(ctxt: Ctxt): Ob = Niv

  def lookupAndInvoke(ctxt: Ctxt): Ob = {
    val fn = meta.lookupObo(this, ctxt.trgt, ctxt)

    fn.invoke(ctxt)
  }

  def getLex(indirect: Boolean, level: Int, offset: Int): Ob = {
    var p = this
    var l = level

    while (l != 0) {
      l -= 1
      p = p.parent.asInstanceOf[Actor]
    }

    if (indirect) {
      p = p.asInstanceOf[Actor].extension
    }

    p.slot.lift(offset).getOrElse(Invalid)
  }

  def setLex(indirect: Boolean, level: Int, offset: Int, value: Ob): Ob = {
    var p = this
    var l = level

    while (l != 0) {
      l -= 1
      p = p.parent.asInstanceOf[Actor]
    }

    if (indirect) {
      p = p.asInstanceOf[Actor].extension
    }

    p.slot.update(offset, value)
    value
  }

}

case class Fixnum(value: Int)    extends Ob
case class Monitor()             extends Ob
case class Symbol(value: String) extends Ob

case object Absent     extends Ob
case object Suicide    extends Ob
case object Invalid    extends Ob
case object Deadthread extends Ob
case object Niv        extends Ob
case object RblFalse   extends Ob
case object RblTrue    extends Ob
