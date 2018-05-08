package coop.rchain.rosette

import cats.MonadError
import cats.data.{ReaderT, State}
import cats.implicits._
import coop.rchain.rosette.Ob.Lenses._
import coop.rchain.rosette.Ob.{getAddr, getLex, setLex}
import coop.rchain.rosette.Ctxt._

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

  /** Try to fetch an `Ob` which is stored at a position described by `loc`
    *
    * TODO: Type signature should be changed to `Reader[GlobalEnv, Option[Ob]]`
    * since fetching a location can't schedule a continuation.
    */
  def fetch(loc: Location): CtxtTransition[Option[Ob]] =
    for {
      ctxt      <- getCtxt
      globalEnv <- getGlobalEnv

      res <- loc match {
              case CtxtRegister(reg) => pureCtxt(ctxt.getReg(reg))

              case ArgRegister(argReg) => pureCtxt(ctxt.argvec.elem.lift(argReg))

              case LexVariable(indirect, level, offset) =>
                pureCtxt(getLex(indirect, level, offset).runA(ctxt.env).value)

              case AddrVariable(indirect, level, offset) =>
                pureCtxt(getAddr(indirect, level, offset).runA(ctxt.env).value)

              case GlobalVariable(offset) => pureCtxt(globalEnv.slot.lift(offset))

              case _: BitField => pureCtxt(None)

              case _: BitField00 => pureCtxt(None)

              // TODO:
              case _ => pureCtxt(None)
            }
    } yield res

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
          .transformS[Ctxt](_.env, (ctxt, env) => ctxt.copy(env = env))

      // TODO:
      case _ => pure(Failure)
    }
  }

  def valWrt[E[_]](loc: Location, v: Ob)(implicit E: MonadError[E, RblError]) =
    ReaderT[E, GlobalEnv, Ob] { globalEnv =>
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
