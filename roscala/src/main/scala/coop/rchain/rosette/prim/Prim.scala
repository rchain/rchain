package coop.rchain.rosette.prim

import coop.rchain.rosette._

import scala.collection.mutable

abstract class Prim extends Ob {
  override val _slot: mutable.Seq[Ob] = null
  val externalName: String
  val minArgs: Int
  val maxArgs: Int

  def fn(ctxt: Ctxt): Either[PrimMismatch, Ob]

  /** Dispatch primitive
    *
    * Rosette seems to potentially return INVALID, UPCALL or DEADTHREAD here
    * Therefore we return RblError for the error case
    */
  def dispatchHelper(ctxt: Ctxt): Result = {
    val n = ctxt.nargs

    if (minArgs <= n && n <= maxArgs) {
      fn(ctxt)
    } else {
      Left(mismatch(ctxt, minArgs, maxArgs))
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

  def mismatch(ctxt: Ctxt, minArgs: Int, maxArgs: Int): PrimMismatch =
    if (maxArgs == Prim.MaxArgs) {
      PrimMismatch(s"expected $minArgs or more arguments")
    } else if (minArgs == maxArgs) {
      if (minArgs == 1) {
        PrimMismatch("expected 1 argument")
      } else {
        PrimMismatch(s"expected $minArgs arguments")
      }
    } else {
      PrimMismatch(s"expected between $minArgs and $maxArgs arguments")
    }

}

object Prim {
  val MaxArgs = 255
  val MaxPrims = 1024

  def nthPrim(n: Int): Option[Prim] = Prims.lift(n)
}
