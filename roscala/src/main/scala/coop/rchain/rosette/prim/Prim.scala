package coop.rchain.rosette.prim

import cats.data.State
import cats.data.State._
import coop.rchain.rosette.Ctxt.{Continuation, CtxtTransition}
import coop.rchain.rosette._
import coop.rchain.rosette.prim.Prim.mismatchArgs

import scala.reflect.{classTag, ClassTag}

sealed trait PrimError
case class ArgumentMismatch(msg: String)               extends PrimError
case class TypeMismatch(argNum: Int, typeName: String) extends PrimError
case class IndexOutOfBounds(argNum: Int, size: Int)    extends PrimError
case object ArithmeticError                            extends PrimError

abstract class Prim extends Ob {
  val name: String
  val minArgs: Int
  val maxArgs: Int

  def fn(ctxt: Ctxt): Either[PrimError, Ob]

  /**
    * Rosette seems to potentially return INVALID, UPCALL or DEADTHREAD here
    * Therefore we return RblError for the error case
    */
  def dispatchHelper: CtxtTransition[Result] = State { ctxt =>
    val n = ctxt.nargs

    if (minArgs <= n && n <= maxArgs)
      (ctxt, fn(ctxt).left.map(PrimErrorWrapper))
    else
      (ctxt, Left(PrimErrorWrapper(mismatchArgs(ctxt, minArgs, maxArgs))))
  }

  /** Dispatch primitive
    *
    * This runs a primitive with dispatchHelper and tries
    * to return the result to the parent ctxt.
    *
    * Returning the result to the parent ctxt, will potentially
    * return the parent ctxt which then has to be scheduled by
    * the caller.
    */
  override def dispatch: CtxtTransition[(Result, Option[Continuation])] = {

    /**
      * Try to return the primitive result to the parent ctxt.
      * This can potentially return the parent ctxt as a ctxt
      * that needs to be scheduled by the caller.
      */
    def returnResultToParent(result: Ob): CtxtTransition[(Result, Option[Continuation])] =
      Ctxt
        .ret(result)
        .transform(
          (ctxt, res) =>
            if (res._1)
              (ctxt, (Right(result), res._2))
            else
              (ctxt, (Right(result), res._2))
        )

    for {
      primResult <- dispatchHelper

      result <- primResult match {
                 case Right(ob) => returnResultToParent(ob)

                 case _ =>
                   /**
                     * Something went wrong with running the primitive.
                     * Report the error back and there is no ctxt to be
                     * scheduled.
                     */
                   pure[Ctxt, (Result, Option[Continuation])]((primResult, None))
               }
    } yield result
  }

  def invoke: CtxtTransition[(Result, Option[Continuation])] = dispatch
}

object Prim {
  val MaxArgs  = 255
  val MaxPrims = 1024

  def mismatchType[T <: Ob: ClassTag](ctxt: Ctxt): Option[TypeMismatch] = {
    val n        = ctxt.nargs
    val typeName = classTag[T].runtimeClass.getName

    val nonT = ctxt.argvec.elem.take(n).find {
      case e: T => false
      case _    => true
    }

    nonT.map(ob => TypeMismatch(ctxt.argvec.elem.indexOf(ob), typeName))
  }

  def mismatchArgs(ctxt: Ctxt, minArgs: Int, maxArgs: Int): ArgumentMismatch = {
    val msg = if (maxArgs == Prim.MaxArgs) {
      s"expected $minArgs or more arguments"
    } else if (minArgs == maxArgs) {
      if (minArgs == 1) {
        "expected 1 argument"
      } else {
        s"expected $minArgs arguments"
      }
    } else {
      s"expected between $minArgs and $maxArgs arguments"
    }

    ArgumentMismatch(msg)
  }

  def nthPrim(n: Int): Option[Prim] = Prims.get(n)
}
