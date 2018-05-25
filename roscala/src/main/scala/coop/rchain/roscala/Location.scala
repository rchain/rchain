package coop.rchain.roscala

import coop.rchain.roscala.ob._

sealed trait Location
case object LocLimbo                                          extends Location
case object LocRslt                                           extends Location
case object LocTrgt                                           extends Location
case class LexVar(level: Int, offset: Int, indirect: Boolean) extends Location
case class GlobalVar(offset: Int)                             extends Location

object Location {
  def store(loc: Location, ctxt: Ctxt, value: Ob): Boolean =
    loc match {
      case LocRslt =>
        ctxt.rslt = value
        false

      case LocTrgt =>
        ctxt.trgt = value
        false

      case _ => true
    }

  def setValWrt(loc: Location, v: Ob, value: Ob)(globalEnv: GlobalEnv): Ob =
    loc match {
      case LexVar(level, offset, indirect) =>
        v.setLex(indirect, level, offset, value)

      case _ =>
        suicide("Location.setValWrt")
        Suicide
    }

  def valWrt(loc: Location, v: Ob)(globalEnv: GlobalEnv): Ob =
    loc match {
      case LexVar(level, offset, indirect) => v.getLex(indirect, level, offset)

      case LocLimbo => Absent

      case _ =>
        suicide("Location.valWrt")
        Suicide
    }
}
