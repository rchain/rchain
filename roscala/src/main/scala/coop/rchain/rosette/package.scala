package coop.rchain

import cats.data.{ReaderWriterState, ReaderWriterStateT, State, StateT}
import cats.implicits._
import cats.{Functor, Monoid}
import coop.rchain.rosette.Ctxt.Continuation
import coop.rchain.rosette.parser.bytecode.ParseError
import coop.rchain.rosette.prim.PrimError

import scala.Function.uncurried
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

package rosette {
  sealed trait RblError
  case object DeadThread                        extends RblError
  case object Invalid                           extends RblError
  case object Suspended                         extends RblError
  case object Absent                            extends RblError
  case object Upcall                            extends RblError
  case object PrimNotFound                      extends RblError
  case class PrimErrorWrapper(value: PrimError) extends RblError
  case class RuntimeError(msg: String)          extends RblError
  case class Suicide(msg: String)               extends RblError

  trait Show[A] {
    def show(a: A): String
  }
}

package object rosette {
  type GlobalEnv = TblObject

  type Result[A] = Either[RblError, A]

  type VMTransition[A] = State[VMState, A]

  type CtxtTransition[A] = ReaderWriterState[Unit, List[Continuation], (GlobalEnv, Ctxt), A]

  def getCtxt = ReaderWriterState.get[Unit, List[Continuation], (GlobalEnv, Ctxt)].map {
    case (_, ctxt) => ctxt
  }

  def getGlobalEnv = ReaderWriterState.get[Unit, List[Continuation], (GlobalEnv, Ctxt)].map {
    case (globalEnv, _) => globalEnv
  }

  def inspectCtxt[A](f: Ctxt => A) =
    ReaderWriterState.inspect[Unit, List[Continuation], (GlobalEnv, Ctxt), A] {
      case (_, ctxt) => f(ctxt)
    }

  def modifyCtxt(f: Ctxt => Ctxt) =
    ReaderWriterState.modify[Unit, List[Continuation], (GlobalEnv, Ctxt)] {
      case (globalEnv, ctxt) => (globalEnv, f(ctxt))
    }

  def modifyGlobalEnv(f: GlobalEnv => GlobalEnv) =
    ReaderWriterState.modify[Unit, List[Continuation], (GlobalEnv, Ctxt)] {
      case (globalEnv, ctxt) => (f(globalEnv), ctxt)
    }

  def tellCont = ReaderWriterState.tell[Unit, List[Continuation], (GlobalEnv, Ctxt)] _

  def pureCtxt[A] = ReaderWriterState.pure[Unit, List[Continuation], (GlobalEnv, Ctxt), A] _

  def liftRWS[F[_]: Functor, E, L: Monoid, S, A](
      st: StateT[F, S, A]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT.applyF[F, E, L, S, A](
      st.runF.map(f =>
        uncurried(_ =>
          s0 =>
            f(s0).map[(L, S, A)] {
              case (s1, a) =>
                (Monoid[L].empty, s1, a)
        }))
    )

  /** Transform a `CtxtTransition` into a `VMTransition`
    *
    * This pulls out continuations from the writer monad in `CtxtTransition`
    * to the return value of the `VMTransition`.
    * It also embeds `ctxt` and `globalEnv` into the `VMTransition`.
    */
  def transformCtxtTransToVMTrans[A](
      trans1: CtxtTransition[A]): VMTransition[(A, List[Continuation])] =
    State { vmState0: VMState =>
      val initial                       = (vmState0.globalEnv, vmState0.ctxt)
      val (conts, (globalEnv, ctxt), a) = trans1.run((), initial).value

      val vmState1 = vmState0.copy(ctxt = ctxt, globalEnv = globalEnv)

      (vmState1, (a, conts))
    }

  implicit class RichOb(ob: Ob) {
    def as[T <: Ob: ClassTag]: Option[T] =
      ob match {
        case t: T => Some(t)
        case _    => None
      }
  }

  implicit class RichCtxtTrans[A](ctxtTrans: State[Ctxt, A]) {
    def embedCtxtInVM: State[VMState, A] =
      ctxtTrans.transformS[VMState](_.ctxt, (vmState, ctxt) => vmState.copy(ctxt = ctxt))
  }

  implicit class OptionOps[R](opt: Option[R]) {
    def or[L](alt: => L): Either[L, R] = opt.toRight(alt)
  }

  def suicide(msg: String): Unit = {
    System.err.println(s"*** fatal error: $msg")
    System.exit(1)
  }

  object Show {
    def apply[A](implicit sh: Show[A]): Show[A] = sh

    def show[A: Show](a: A): String = Show[A].show(a)

    implicit class ShowOps[A: Show](a: A) {
      def show: String = Show[A].show(a)
    }

    implicit val parseErrorShow: Show[ParseError] = _.toString

    implicit val opsShow: Show[Seq[Op]] = { ops =>
      ops
        .map { op =>
          val r = currentMirror.reflect(op)

          val name = op.getClass.getSimpleName
          val args = r.symbol.typeSignature.members.toStream
            .collect { case s: TermSymbol if !s.isMethod => r.reflectField(s) }
            .map { r =>
              r.symbol.name.toString.trim + ":" + r.get
            }

          name + " " + args.mkString(" ")
        }
        .mkString("\n")
    }
  }
}
