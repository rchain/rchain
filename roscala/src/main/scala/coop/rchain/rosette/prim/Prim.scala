package coop.rchain.rosette.prim

import cats.implicits._
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

  override val meta   = null
  override val parent = null

  def fn: CtxtTransition[PrimResult] =
    for {
      ctxt       <- getCtxt
      globalEnv  <- getGlobalEnv
      primResult = fnSimple(ctxt)
    } yield primResult

  def fnSimple(ctxt: Ctxt): PrimResult = ???

  def dispatchHelper: CtxtTransition[Result[Ob]] =
    for {
      ctxt      <- getCtxt
      globalEnv <- getGlobalEnv
      n         = ctxt.nargs

      result <- if (minArgs <= n && n <= maxArgs)
                 fn.transform((writer, state, res) =>
                   (writer, state, res.left.map(PrimErrorWrapper)))
               else
                 pureCtxt[Result[Ob]](Left(PrimErrorWrapper(mismatchArgs(ctxt, minArgs, maxArgs))))
    } yield result

  /** Dispatch primitive
    *
    * This runs a primitive with dispatchHelper and tries
    * to return the result to the parent `ctxt`.
    *
    * Returning the result to the parent `ctxt` will potentially
    * return the parent `ctxt` which then has to be scheduled by
    * the caller.
    *
    * Running a primitive through `dispatchHelper` can also potentially
    * return a ctxt that has to be scheduled (see `ctxt-rtn`). Therefore `dispatchPrim`
    * returns a `List[Continuation]`.
    */
  override def dispatch: CtxtTransition[Result[Ob]] =
    for {
      primResult <- dispatchHelper

      result <- primResult match {

                 /**
                   * Return the primitive result to the continuation
                   * of the current `ctxt`.
                   */
                 case Right(res) => Ctxt.ret(res).map(_ => primResult)

                 case error =>
                   /**
                     * Something went wrong with running the primitive.
                     * Report the error back.
                     */
                   pureCtxt[Result[Ob]](error)
               }
    } yield result

  def invoke: CtxtTransition[Result[Ob]] = dispatch //TODO
}

object Prim {
  val MaxArgs  = 255
  val MaxPrims = 1024

  def mismatchType[T <: Ob: ClassTag](ctxt: Ctxt): Option[TypeMismatch] = {
    val n        = ctxt.nargs
    val typeName = classTag[T].runtimeClass.getName

    val nonT = ctxt.argvec.elem.take(n).find {
      case _: T => false
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
