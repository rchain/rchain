package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.prim.fixnum._
import coop.rchain.roscala.ob._
import org.scalatest._

class FixnumSpec extends FlatSpec with Matchers {
  val globalEnv = new GlobalEnv()

  val state = State(globalEnv = globalEnv)(strandPool = null)

  val ctxt = new Ctxt(
    tag = null,
    nargs = 1,
    pc = 0,
    argvec = Tuple(Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  ctxt.trgt = null
  ctxt.rslt = null

  def reAssignCtxtArgs(ctxt: Ctxt, nargs: Int, args: Tuple): Ctxt = {
    val newCtxt = ctxt.clone()
    newCtxt.nargs = nargs
    newCtxt.argvec = args
    newCtxt
  }

  "fxPlus" should "correctly add numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Fixnum(1)))
    fxPlus.fn(newCtxt, state) should be(Fixnum(5))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    fxPlus.fn(newCtxt, state) should be(Deadthread)
  }

  "fxMinus" should "correctly subtract numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(1)))
    fxMinus.fn(newCtxt, state) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    fxMinus.fn(newCtxt, state) should be(Deadthread)
  }

  "fxTimes" should "correctly multiply numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, Fixnum(5)))
    fxTimes.fn(newCtxt, state) should be(Fixnum(125))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, Niv))
    fxTimes.fn(newCtxt, state) should be(Deadthread)
  }

  "fxDiv" should "correctly divide numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(5)))
    fxDiv.fn(newCtxt, state) should be(Fixnum(1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxDiv.fn(newCtxt, state) should be(Deadthread)
  }

  "fxMod" should "correctly return the remainder" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(5)))
    fxMod.fn(newCtxt, state) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMod.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxLt.fn(newCtxt, state) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxLt.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLt.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(2)))
    fxLe.fn(newCtxt, state) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxLe.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLe.fn(newCtxt, state) should be(Deadthread)
  }

  "fxGt" should "correctly return whether former greater than latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(2)))
    fxGt.fn(newCtxt, state) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxGt.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxGt.fn(newCtxt, state) should be(Deadthread)
  }

  "fxGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(5)))
    fxGe.fn(newCtxt, state) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxGe.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxGe.fn(newCtxt, state) should be(Deadthread)
  }

  "fxEq" should "correctly return whether former equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(5)))
    fxEq.fn(newCtxt, state) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxEq.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxEq.fn(newCtxt, state) should be(Deadthread)
  }

  "fxNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(5)))
    fxNe.fn(newCtxt, state) should be(RblBool(false))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxNe.fn(newCtxt2, state) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxNe.fn(newCtxt, state) should be(Deadthread)
  }

  "fxMin" should "correctly return the smallest one" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(Fixnum(2), Fixnum(2), Fixnum(2), Fixnum(5)))
    fxMin.fn(newCtxt, state) should be(Fixnum(2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMin.fn(newCtxt, state) should be(Deadthread)
  }

  "fxMax" should "correctly return the greatest one" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(Fixnum(2), Fixnum(2), Fixnum(2), Fixnum(5)))
    fxMax.fn(newCtxt, state) should be(Fixnum(5))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMax.fn(newCtxt, state) should be(Deadthread)
  }

  "fxAbs" should "correctly return absolute value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(2)))
    fxAbs.fn(newCtxt, state) should be(Fixnum(2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAbs.fn(newCtxt, state) should be(Deadthread)
  }

  "fxExpt" should "correctly return base-number raised to the power power-number" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxExpt.fn(newCtxt, state) should be(Fixnum(32))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxExpt.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLg" should "correctly compute Ceil(ln(input_value))" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLg.fn(newCtxt, state) should be(Fixnum(4))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(-1)))
    fxLg.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLgf" should "correctly compute Floor(ln(input_value))" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLgf.fn(newCtxt, state) should be(Fixnum(3))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(-1)))
    fxLgf.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLogand" should "correctly return the bitwise 'and' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(8), Fixnum(4), Fixnum(2)))
    fxLogand.fn(newCtxt, state) should be(Fixnum(0))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogand.fn(newCtxt2, state) should be(Fixnum(~0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogand.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLogor" should "correctly return the bitwise 'or' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(8), Fixnum(4), Fixnum(2)))
    fxLogor.fn(newCtxt, state) should be(Fixnum(14))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogor.fn(newCtxt2, state) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogor.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLogxor" should "correctly return the bitwise 'xor' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(6), Fixnum(4), Fixnum(2)))
    fxLogxor.fn(newCtxt, state) should be(Fixnum(0))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogxor.fn(newCtxt2, state) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogxor.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLognot" should "correctly compute bitwise '!' of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLognot.fn(newCtxt, state) should be(Fixnum(-11))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLognot.fn(newCtxt, state) should be(Deadthread)
  }

  "fxMdiv" should "correctly return former value module division latter value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxMdiv.fn(newCtxt, state) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxMdiv.fn(newCtxt, state) should be(Deadthread)
  }

  "fxCdiv" should "correctly compute Ceil(n/m)" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(2)))
    fxCdiv.fn(newCtxt, state) should be(Fixnum(3))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxCdiv.fn(newCtxt, state) should be(Deadthread)
  }

  "fxAsl" should "correctly compute arithmetical left shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(2)))
    fxAsl.fn(newCtxt, state) should be(Fixnum(20))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAsl.fn(newCtxt, state) should be(Deadthread)
  }

  "fxAsr" should "correctly compute arithmetical right shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(2)))
    fxAsr.fn(newCtxt, state) should be(Fixnum(-1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAsr.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLsl" should "correctly compute logical left shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(1)))
    fxLsl.fn(newCtxt, state) should be(Fixnum(-2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLsl.fn(newCtxt, state) should be(Deadthread)
  }

  "fxLsr" should "correctly compute logical right shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(31)))
    fxLsr.fn(newCtxt, state) should be(Fixnum(1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLsr.fn(newCtxt, state) should be(Deadthread)
  }
}
