package coop.rchain.rosette

sealed trait Location
case class LocationAtom(atom: Ob) extends Location
case class LocationGT(genericType: Location.GenericType) extends Location

object Location {
  import Ob.Lenses._

  val AddrLevelSize = 5
  val AddrOffsetSize = 14
  val ArgRegIndexSize = 8
  val BitFieldLevelSize = 4
  val BitFieldOffsetSize = 12
  val BitFieldSpanSize = 5
  val BitField00OffsetSize = 17
  val BitField00SpanSize = 5
  val GlobalOffsetSize = 16
  val LexLevelSize = 5
  val LexOffsetSize = 14
  val MaxArgs = (1 << ArgRegIndexSize) - 1
  val NumberOfCtxtRegs = 10

  object PLACEHOLDER extends LocationGT(LTLimbo)
  object LIMBO extends LocationGT(LTLimbo)

  sealed trait GenericType
  case class LTCtxtRegister(reg: Int) extends GenericType
  case class LTArgRegister(argReg: Int) extends GenericType
  case class LTLexVariable(indirect: Boolean, level: Int, offset: Int)
      extends GenericType
  case class LTAddrVariable(indirect: Boolean, level: Int, offset: Int)
      extends GenericType
  case class LTGlobalVariable(offset: Int) extends GenericType
  case class LTBitField(indirect: Boolean,
                        level: Int,
                        offset: Int,
                        spanSize: Int,
                        sign: Int = 0)
      extends GenericType
  case class LTBitField00(offset: Int, spanSize: Int, sign: Int)
      extends GenericType
  case object LTLimbo extends GenericType

  def isFixNum(value: Ob): Boolean = false
  def fixVal(value: Ob): Int = 0

  sealed trait StoreResult
  case object StoreFail extends StoreResult
  case class StoreCtxt(ctxt: Ctxt) extends StoreResult
  case class StoreGlobal(globalEnv: TblObject) extends StoreResult

  def store(loc: Location,
            k: Ctxt,
            globalEnv: TblObject,
            value: Ob): StoreResult =
    loc match {
      case LocationAtom(_) => StoreFail
      case LocationGT(genericType) =>
        genericType match {
          case LTCtxtRegister(reg) =>
            k.setReg(reg, value) match {
              case Some(newCtxt) => StoreCtxt(newCtxt)
              case None => StoreFail
            }

          case LTArgRegister(argReg) =>
            if (argReg < k.argvec.elem.size) {
              StoreCtxt(
                k.update(_ >> 'argvec >> 'elem)(_.updated(argReg, value)))
            } else {
              StoreFail
            }

          case LTLexVariable(indirect, level, offset) =>
            k.env.setLex(indirect, level, offset, value) match {
              case (env, value) if value != Ob.INVALID =>
                StoreCtxt(k.set(_ >> 'env)(env))
              case _ => StoreFail
            }

          case LTAddrVariable(indirect, level, offset) =>
            k.env.setAddr(indirect, level, offset, value) match {
              case env if env != Ob.INVALID => StoreCtxt(k.set(_ >> 'env)(env))
              case _ => StoreFail
            }

          case LTGlobalVariable(offset) =>
            if (offset < globalEnv.numberOfSlots()) {
              StoreGlobal(
                globalEnv.update(_ >> 'slot)(_.updated(offset, value)))
            } else {
              StoreFail
            }

          case LTBitField(indirect, level, offset, spanSize, sign) =>
            if (!isFixNum(value)) {
              k.env.setField(indirect, level, offset, spanSize, fixVal(value)) match {
                case env if env != Ob.INVALID =>
                  StoreCtxt(k.set(_ >> 'env)(env))
                case _ => StoreFail
              }
            } else {
              StoreFail
            }

          case LTBitField00(offset, spanSize, sign) =>
            if (!isFixNum(value)) {
              k.env.setField(indirect = false,
                             0,
                             offset,
                             spanSize,
                             fixVal(value)) match {
                case env if env != Ob.INVALID =>
                  StoreCtxt(k.set(_ >> 'env)(env))
                case _ => StoreFail
              }
            } else {
              StoreFail
            }

          case LTLimbo => StoreFail
        }
    }

  def fetch(loc: Location, k: Ctxt, globalEnv: TblObject): Ob =
    loc match {
      case LocationAtom(_) => Ob.INVALID
      case LocationGT(genericType) =>
        genericType match {
          case LTCtxtRegister(reg) =>
            if (reg < NumberOfCtxtRegs) {
              k.getReg(reg).getOrElse(Ob.INVALID)
            } else {
              Ob.INVALID
            }

          case LTArgRegister(argReg) =>
            if (argReg < k.argvec.elem.size) {
              k.argvec.elem(argReg)
            } else {
              Ob.INVALID
            }

          case LTLexVariable(indirect, level, offset) =>
            k.env.getLex(indirect, level, offset)

          case LTAddrVariable(indirect, level, offset) =>
            k.env.getAddr(indirect, level, offset)

          case LTGlobalVariable(offset) =>
            if (offset < globalEnv.numberOfSlots()) {
              globalEnv.slot(offset)
            } else {
              Ob.INVALID
            }

          case LTBitField(indirect, level, offset, spanSize, sign) =>
            k.env.getField(indirect, level, offset, spanSize, sign)

          case LTBitField00(offset, spanSize, sign) =>
            k.env.getField(false, 0, offset, spanSize, sign)

          case LTLimbo => Ob.INVALID
        }
    }

  def printRep(loc: Location): String = {
    val names = List("rslt",
                     "trgt",
                     "argvec",
                     "env",
                     "code",
                     "ctxt",
                     "self",
                     "self-env",
                     "rcvr",
                     "monitor")

    loc match {
      case LocationAtom(_) => {
        suicide("Location.printRep: not a generic type")
        ""
      }

      case LocationGT(genericType) =>
        genericType match {
          case LTCtxtRegister(reg) =>
            if (0 <= reg && reg < NumberOfCtxtRegs) {
              names(reg)
            } else {
              f"unknown ctxt register 0x$reg%x"
            }

          case LTArgRegister(argReg) =>
            s"arg[$argReg]"

          case LTLexVariable(indirect, level, offset) => {
            val offsetStr = if (indirect) s"($offset)" else s"$offset"
            s"lex[$level,$offsetStr]"
          }

          case LTAddrVariable(indirect, level, offset) => {
            val offsetStr = if (indirect) s"($offset)" else s"$offset"
            s"addr[$level,$offsetStr]"
          }

          case LTGlobalVariable(offset) =>
            s"global[$offset]"

          case LTBitField(indirect, level, offset, spanSize, sign) => {
            val signStr = if (sign != 0) "s" else "u"
            val offsetStr = if (indirect) s"($offset)" else s"$offset"
            s"${signStr}fld[$level,$offsetStr,$spanSize]"
          }

          case LTBitField00(offset, spanSize, sign) => {
            val signStr = if (sign != 0) "s" else "u"
            s"${signStr}fld[$offset,$spanSize]"
          }

          case LTLimbo => "limbo"
        }
    }
  }

  def valWRT(loc: Location, v: Ob, globalEnv: TblObject): Ob =
    loc match {
      case LocationAtom(_) => {
        suicide(s"Location.valWrt: $loc")
        null
      }

      case LocationGT(genericType) =>
        genericType match {
          case LTLexVariable(indirect, level, offset) =>
            v.getLex(indirect, level, offset)

          case LTAddrVariable(indirect, level, offset) =>
            v.getAddr(indirect, level, offset)

          case LTGlobalVariable(offset) =>
            globalEnv.getLex(indirect = true, 0, offset)

          case LTBitField(indirect, level, offset, spanSize, sign) =>
            v.getField(indirect, level, offset, spanSize, sign)

          case LTBitField00(offset, spanSize, sign) =>
            v.getField(indirect = false, 0, offset, spanSize, sign)

          case LTLimbo => Ob.ABSENT

          case _ => {
            suicide(s"Location.valWrt: $loc")
            null
          }
        }
    }

  def setValWrt(loc: Location, v: Ob, globalEnv: TblObject, value: Ob): Ob =
    loc match {
      case LocationAtom(_) => {
        suicide(s"Location.setValWrt: $loc")
        null
      }

      case LocationGT(genericType) =>
        genericType match {
          case LTLexVariable(indirect, level, offset) =>
            v.setLex(indirect, level, offset, value)._2

          case LTAddrVariable(indirect, level, offset) =>
            v.setAddr(indirect, level, offset, value)

          case LTGlobalVariable(offset) =>
            globalEnv.setLex(indirect = true, 0, offset, value)._2

          case LTBitField(indirect, level, offset, spanSize, sign) =>
            v.setField(indirect, level, offset, spanSize, fixVal(value))

          case LTBitField00(offset, spanSize, sign) =>
            v.setField(indirect = false, 0, offset, spanSize, fixVal(value))

          case _ => {
            suicide(s"Location.setValWrt: $loc")
            null
          }
        }
    }

  def adjustLevel(loc: Location, adjustment: Int): Location =
    loc match {
      case LocationAtom(_) => {
        suicide(s"Location.adjustLevel: $loc")
        null
      }

      case LocationGT(genericType) =>
        LocationGT(genericType match {
          case LTLexVariable(indirect, level, offset) =>
            LTLexVariable(indirect, level + adjustment, offset)

          case LTAddrVariable(indirect, level, offset) =>
            LTAddrVariable(indirect, level + adjustment, offset)

          case LTBitField(indirect, level, offset, spanSize, sign) =>
            LTBitField(indirect, level + adjustment, offset, spanSize, sign)

          case _ => {
            suicide(s"Location.adjustLevel: $loc")
            null
          }
        })
    }

  def CtxtReg(n: Int): Location = {
    if (n >= NumberOfCtxtRegs) {
      suicide(s"Location.CtxtReg: invalid ctxt register ($n)");
      null
    }

    LocationGT(LTCtxtRegister(n))
  }

  def ArgReg(n: Int): Location = {
    if (n > MaxArgs) {
      suicide(s"Location.ArgReg: invalid arg register index ($n)")
    }

    LocationGT(LTArgRegister(n))
  }

  def LexVar(level: Int, offset: Int, indirect: Boolean): Location = {
    if (level >= (1 << LexLevelSize) || offset >= (1 << LexOffsetSize)) {
      val offsetStr = if (indirect) s"($offset)" else s"$offset"
      suicide(
        s"Location.LexVar: unrepresentable location (lex[$level,$offsetStr])")
      null
    }
    LocationGT(LTLexVariable(indirect, level, offset))
  }

  def AddrVar(level: Int, offset: Int, indirect: Boolean): Location = {
    if (level >= (1 << AddrLevelSize) || offset >= (1 << AddrOffsetSize)) {
      val offsetStr = if (indirect) s"($offset)" else s"$offset"
      suicide(
        s"Location.AddrVar: unrepresentable location (addr[$level,$offsetStr])")
      null
    }
    LocationGT(LTAddrVariable(indirect, level, offset))
  }

  def GlobalVar(n: Int): Location = {
    if (n >= (1 << GlobalOffsetSize)) {
      suicide(s"Location.GlobalVar: unrepresentable location (global[$n])")
      null
    }

    LocationGT(LTGlobalVariable(n))
  }

  def BitField(level: Int,
               offset: Int,
               span: Int,
               indirect: Boolean,
               sign: Int): Location = {
    if (level >= (1 << BitFieldLevelSize)
        || offset >= (1 << BitFieldOffsetSize)
        || span > (1 << BitFieldSpanSize)) {
      val offsetStr = if (indirect) s"($offset)" else s"$offset"
      val signStr = if (sign == 0) "u" else "s"
      suicide(
        s"Location.BitField: unrepresentable location (${signStr}fld[$level,$offsetStr,$span])")
      null
    }

    LocationGT(
      LTBitField(indirect,
                 level,
                 offset,
                 span % (1 << BitFieldSpanSize),
                 if (sign == 0) 0 else 1))
  }

  def BitField00(offset: Int, span: Int, sign: Int): Location = {
    if (offset >= (1 << BitField00OffsetSize)
        || span > (1 << BitField00SpanSize)) {
      val signStr = if (sign == 0) "u" else "s"
      suicide(
        s"Location.BitField: unrepresentable location (${signStr}fld[$offset,$span])")
      null
    }

    LocationGT(
      LTBitField00(offset,
                   span % (1 << BitField00SpanSize),
                   if (sign == 0) 0 else 1))
  }

  def Limbo(): Location = LocationGT(LTLimbo)

  val LocLimbo = Limbo()

  val LocRslt = CtxtReg(CtxtRegName.rslt)
  val LocTrgt = CtxtReg(CtxtRegName.trgt)
}
