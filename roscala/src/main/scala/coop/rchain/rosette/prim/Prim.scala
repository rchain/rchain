package coop.rchain.rosette.prim

import coop.rchain.rosette._
import coop.rchain.rosette.prim.Prim.mismatchArgs

import scala.reflect.{classTag, ClassTag}

sealed trait PrimError
case class ArgumentMismatch(msg: String) extends PrimError
case class TypeMismatch(argNum: Int, typeName: String) extends PrimError
case object ArithmeticError extends PrimError

abstract class Prim extends Ob {
  val name: String
  val minArgs: Int
  val maxArgs: Int

  def fn(ctxt: Ctxt): Either[PrimError, Ob]

  /** Dispatch primitive
    *
    * Rosette seems to potentially return INVALID, UPCALL or DEADTHREAD here
    * Therefore we return RblError for the error case
    */
  def dispatchHelper(ctxt: Ctxt): Result = {
    val n = ctxt.nargs

    if (minArgs <= n && n <= maxArgs) {
      fn(ctxt).left.map(PrimErrorWrapper)
    } else {
      Left(PrimErrorWrapper(mismatchArgs(ctxt, minArgs, maxArgs)))
    }
  }

  override def dispatch(state: VMState): (Result, VMState) = {
    // TODO:
    //if (debugging_level)
    //    printf("\t%s\n", BASE(id)->asCstring());

    val result = dispatchHelper(state.ctxt)

    if (result.isRight) {
      val r = result.right.get
      (Right(r), state.ctxt.ret(r)(state)._2)
    } else {
      (result, state)
    }
  }

  def invoke(state: VMState): (Result, VMState) = {
    val result = dispatch(state)

    // TODO:
    //if (!BASE(ctxt->trgt)->isSynchronousTrgt())
    //    BASE(ctxt->arg(0))->updateNoArgs();

    result
  }

}

object Prim {
  val MaxArgs = 255
  val MaxPrims = 1024

  def mismatchType[T <: Ob: ClassTag](ctxt: Ctxt): Option[TypeMismatch] = {
    val n = ctxt.nargs
    val typeName = classTag[T].runtimeClass.getName

    val nonT = ctxt.argvec.elem.take(n).find {
      case e: T => false
      case _ => true
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
