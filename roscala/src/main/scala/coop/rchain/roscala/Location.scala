package coop.rchain.roscala

import coop.rchain.roscala.ob._

sealed trait Location extends Ob
case class ArgRegister(arg: Int) extends Location {
  override def toString: String = s"ArgRegister(arg = $arg)"
}
case class CtxtRegister(reg: Int) extends Location {
  override def toString: String = s"CtxtRegister(reg = ${regName(reg)})"
}
case class GlobalVar(offset: Int) extends Location {
  override def toString: String = s"GlobalVar(offset = $offset)"
}
case class LexVariable(level: Int, offset: Int, indirect: Boolean) extends Location {
  override def toString: String = s"LexVar(level = $level, offset = $offset, indirect = $indirect)"
}
case object LocLimbo extends Location
case object LocRslt  extends Location
case object LocTrgt  extends Location

object Location {
  val NumberOfCtxtRegs = 10

  def store(loc: Location, ctxt: Ctxt, value: Ob): Boolean =
    loc match {
      case CtxtRegister(reg) =>
        ctxt.setReg(reg, value)
        false

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

  def fetch(loc: Location, ctxt: Ctxt, globalEnv: GlobalEnv): Ob =
    loc match {
      case CtxtRegister(reg) =>
        if (reg < NumberOfCtxtRegs)
          ctxt.reg(reg)
        else
          Invalid

      case ArgRegister(arg) =>
        if (arg < ctxt.argvec.numberOfElements())
          ctxt.arg(arg)
        else
          Invalid

      case LexVariable(level, offset, indirect) =>
        ctxt.env.getLex(indirect, level, offset)

      case GlobalVar(offset) =>
        if (offset < globalEnv.numberOfSlots)
          globalEnv.slot.unsafeGet(offset)
        else
          Invalid

      case _ => LocLimbo
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
