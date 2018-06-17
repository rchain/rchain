package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.prim.fixnum._
import coop.rchain.roscala.ob._
import org.scalatest._

class FixnumSpec extends FlatSpec with Matchers {
  val globalEnv = new GlobalEnv()
  val ctxt = new Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = 0,
    rslt = null,
    trgt = null,
    argvec = Tuple(Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null,
  )

  def reAssignCtxtArgs(ctxt: Ctxt, nargs: Int, args: Tuple): Ctxt = {
    val newCtxt = ctxt.clone()
    newCtxt.nargs = nargs
    newCtxt.argvec = args
    newCtxt
  }

  "fxPlus" should "correctly add numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Fixnum(1)))
    fxPlus.fn(newCtxt, globalEnv) should be(Fixnum(5))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    fxPlus.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxMinus" should "correctly subtract numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(1)))
    fxMinus.fn(newCtxt, globalEnv) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    fxMinus.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxTimes" should "correctly multiply numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, Fixnum(5)))
    fxTimes.fn(newCtxt, globalEnv) should be(Fixnum(125))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, Niv))
    fxTimes.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxDiv" should "correctly divide numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(5)))
    fxDiv.fn(newCtxt, globalEnv) should be(Fixnum(1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxDiv.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxMod" should "correctly return the remainder" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Fixnum(5)))
    fxMod.fn(newCtxt, globalEnv) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMod.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxLt.fn(newCtxt, globalEnv) should be(RblBool(true))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    fxLt.fn(newCtxt2, globalEnv) should be(RblBool(false))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLt.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxMin" should "correctly return the smallest one" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(Fixnum(2), Fixnum(2), Fixnum(2), Fixnum(5)))
    fxMin.fn(newCtxt, globalEnv) should be(Fixnum(2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMin.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxMax" should "correctly return the greatest one" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(Fixnum(2), Fixnum(2), Fixnum(2), Fixnum(5)))
    fxMax.fn(newCtxt, globalEnv) should be(Fixnum(5))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxMax.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxAbs" should "correctly return absolute value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(2)))
    fxAbs.fn(newCtxt, globalEnv) should be(Fixnum(2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAbs.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxExpt" should "correctly return base-number raised to the power power-number" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxExpt.fn(newCtxt, globalEnv) should be(Fixnum(32))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxExpt.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLg" should "correctly compute Ceil(ln(input_value))" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLg.fn(newCtxt, globalEnv) should be(Fixnum(4))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(-1)))
    fxLg.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLgf" should "correctly compute Floor(ln(input_value))" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLgf.fn(newCtxt, globalEnv) should be(Fixnum(3))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(-1)))
    fxLgf.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLogand" should "correctly return the bitwise 'and' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(8), Fixnum(4), Fixnum(2)))
    fxLogand.fn(newCtxt, globalEnv) should be(Fixnum(0))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogand.fn(newCtxt2, globalEnv) should be(Fixnum(~0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogand.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLogor" should "correctly return the bitwise 'or' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(8), Fixnum(4), Fixnum(2)))
    fxLogor.fn(newCtxt, globalEnv) should be(Fixnum(14))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogor.fn(newCtxt2, globalEnv) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogor.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLogxor" should "correctly return the bitwise 'xor' of all input values" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Fixnum(6), Fixnum(4), Fixnum(2)))
    fxLogxor.fn(newCtxt, globalEnv) should be(Fixnum(0))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    fxLogxor.fn(newCtxt2, globalEnv) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    fxLogxor.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLognot" should "correctly compute bitwise '!' of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Fixnum(10)))
    fxLognot.fn(newCtxt, globalEnv) should be(Fixnum(-11))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLognot.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxMdiv" should "correctly return former value module division latter value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Fixnum(5)))
    fxMdiv.fn(newCtxt, globalEnv) should be(Fixnum(0))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxMdiv.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxCdiv" should "correctly compute Ceil(n/m)" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(2)))
    fxCdiv.fn(newCtxt, globalEnv) should be(Fixnum(3))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxCdiv.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxAsl" should "correctly compute arithmetical left shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(5), Fixnum(2)))
    fxAsl.fn(newCtxt, globalEnv) should be(Fixnum(20))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAsl.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxAsr" should "correctly compute arithmetical right shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(2)))
    fxAsr.fn(newCtxt, globalEnv) should be(Fixnum(-1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxAsr.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLsl" should "correctly compute logical left shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(1)))
    fxLsl.fn(newCtxt, globalEnv) should be(Fixnum(-2))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLsl.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "fxLsr" should "correctly compute logical right shift of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(-1), Fixnum(31)))
    fxLsr.fn(newCtxt, globalEnv) should be(Fixnum(1))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    fxLsr.fn(newCtxt, globalEnv) should be(Deadthread)
  }
}
