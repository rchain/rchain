package coop.rchain.rosette

import cats.MonadError
import cats.data.{ReaderT, State}
import coop.rchain.rosette.Ctxt.setReg
import coop.rchain.rosette.Ob.Lenses._
import coop.rchain.rosette.Ob.{getAddr, getLex, setLex}

sealed trait Location extends Ob {
  override val meta   = null
  override val parent = null
}
case class ArgRegister(argReg: Int)                                 extends Location
case class CtxtRegister(reg: Int)                                   extends Location
case class AddrVariable(indirect: Boolean, level: Int, offset: Int) extends Location
case class BitField(indirect: Boolean, level: Int, offset: Int, spanSize: Int, sign: Int = 0)
    extends Location
case class BitField00(offset: Int, spanSize: Int, sign: Int)       extends Location
case class GlobalVariable(offset: Int)                             extends Location
case class LexVariable(indirect: Boolean, level: Int, offset: Int) extends Location
case object Limbo                                                  extends Location

sealed trait StoreResult
case object Success extends StoreResult
case object Failure extends StoreResult

object Location {
  val LocRslt = CtxtRegister(0)
  val LocTrgt = CtxtRegister(1)

  def fetch(loc: Location, globalEnv: TblObject): State[Ctxt, Option[Ob]] = {
    val pure = State.pure[Ctxt, Option[Ob]] _

    for {
      ctxt <- State.get[Ctxt]
      res <- loc match {
              case CtxtRegister(reg) => pure(ctxt.getReg(reg))

              case ArgRegister(argReg) => pure(ctxt.argvec.elem.lift(argReg))

              case LexVariable(indirect, level, offset) =>
                pure(getLex(indirect, level, offset).runA(ctxt.env).value)

              case AddrVariable(indirect, level, offset) =>
                pure(getAddr(indirect, level, offset).runA(ctxt.env).value)

              case GlobalVariable(offset) => pure(globalEnv.slot.lift(offset))

              case BitField(indirect, level, offset, spanSize, sign) => pure(None)

              case BitField00(offset, spanSize, sign) => pure(None)

              // TODO:
              case _ => pure(None)
            }
    } yield res
  }

  def store(loc: Location, value: Ob): State[Ctxt, StoreResult] = {
    val pure = State.pure[Ctxt, StoreResult] _

    loc match {
      case CtxtRegister(reg) => setReg(reg, value)

      case ArgRegister(argReg) =>
        State { ctxt: Ctxt =>
          if (argReg >= ctxt.argvec.elem.size)
            (ctxt, Failure)
          else
            (ctxt.update(_ >> 'argvec >> 'elem)(_.updated(argReg, value)), Success)
        }

      case LexVariable(indirect, level, offset) =>
        setLex(indirect, level, offset, value)
          .transformS(_.env, (ctxt, env) => ctxt.copy(env = env))

      // TODO:
      case _ => pure(Failure)
    }
  }

  def valWrt[E[_]](loc: Location, v: Ob)(implicit E: MonadError[E, RblError]) =
    ReaderT[E, TblObject, Ob] { globalEnv =>
      val value: PartialFunction[Location, Ob] = {
        case LexVariable(indirect, level, offset) =>
          v.getLex(indirect, level, offset)
        case AddrVariable(indirect, level, offset) =>
          v.getAddr(indirect, level, offset)
        case BitField(indirect, level, offset, spanSize, sign) =>
          v.getField(indirect, level, offset, spanSize, sign)
        case BitField00(offset, spanSize, sign) =>
          v.getField(indirect = false, level = 0, offset, spanSize, sign)
        case GlobalVariable(offset) =>
          globalEnv.getLex(indirect = true, level = 0, offset)
      }

      val err: Location => RblError = {
        case Limbo => Absent
        case _     => Suicide("valWrt(Location, Ob*)")
      }

      (value andThen E.pure) applyOrElse (loc, err andThen E.raiseError[Ob])
    }
}
