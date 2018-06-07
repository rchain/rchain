package coop.rchain.roscala

import coop.rchain.roscala.ob._

sealed trait Location                                              extends Ob
case class ArgRegister(arg: Int)                                   extends Location
case class GlobalVar(offset: Int)                                  extends Location
case class LexVariable(level: Int, offset: Int, indirect: Boolean) extends Location
case object LocLimbo                                               extends Location
case object LocRslt                                                extends Location
case object LocTrgt                                                extends Location

object Location {
  def store(loc: Location, ctxt: Ctxt, value: Ob): Boolean =
    loc match {
      case LocRslt =>
        ctxt.rslt = value
        false

      case LocTrgt =>
        ctxt.trgt = value
        false

      case ArgRegister(arg) =>
        if (arg >= ctxt.argvec.numberOfElements())
          true
        else {
          ctxt.argvec.update(arg, value)
          false
        }

      case LexVariable(level, offset, indirect) =>
        ctxt.env.setLex(indirect, level, offset, value) == Invalid

      case _ => true
    }

  def setValWrt(loc: Location, v: Ob, value: Ob)(globalEnv: GlobalEnv): Ob =
    loc match {
      case LexVariable(level, offset, indirect) =>
        v.setLex(indirect, level, offset, value)

      case _ =>
        suicide("Location.setValWrt")
        Suicide
    }

  def valWrt(loc: Location, v: Ob)(globalEnv: GlobalEnv): Ob =
    loc match {
      case LexVariable(level, offset, indirect) => v.getLex(indirect, level, offset)

      case LocLimbo => Absent

      case _ =>
        suicide("Location.valWrt")
        Suicide
    }
}
