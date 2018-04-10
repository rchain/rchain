package coop.rchain.rosette

import cats.Eval
import cats.data.{ReaderWriterState, ReaderWriterStateT, State}
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

              case BitField(_, _, _, _, _) => pureCtxt(None)

              case BitField00(_, _, _) => pureCtxt(None)

              // TODO:
              case _ => pureCtxt(None)
            }
    } yield res

  def store(loc: Location, value: Ob): State[Ctxt, StoreResult] =
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
      case _ => State.pure[Ctxt, StoreResult](Failure)
    }
}
