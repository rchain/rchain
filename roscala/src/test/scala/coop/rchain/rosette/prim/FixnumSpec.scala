package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.fixnum._
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, PC, RblBool, Tuple}
import org.scalatest._

class FixnumSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(1, Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null,
  )

  "fxPlus" should "correctly add numbers" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Fixnum(1)))
    fxPlus.fnSimple(newCtxt) should be(Right(Fixnum(5)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    fxPlus.fnSimple(newCtxt) should be('left)
  }

  "fxMinus" should "correctly subtract numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(1)))
    fxMinus.fnSimple(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    fxMinus.fnSimple(newCtxt) should be('left)
  }

  "fxTimes" should "correctly multiply numbers" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Fixnum(5)))
    fxTimes.fnSimple(newCtxt) should be(Right(Fixnum(125)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Ob.NIV))
    fxTimes.fnSimple(newCtxt) should be('left)
  }

  "fxDiv" should "correctly divide numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(5)))
    fxDiv.fnSimple(newCtxt) should be(Right(Fixnum(1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxDiv.fnSimple(newCtxt) should be('left)
  }

  "fxMod" should "correctly return the remainder" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(5)))
    fxMod.fnSimple(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMod.fnSimple(newCtxt) should be('left)
  }

  "fxLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxLt.fnSimple(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxLt.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLt.fnSimple(newCtxt) should be('left)
  }

  "fxLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(2))))
    fxLe.fnSimple(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxLe.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLe.fnSimple(newCtxt) should be('left)
  }

  "fxGt" should "correctly return whether former greater than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxGt.fnSimple(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxGt.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxGt.fnSimple(newCtxt) should be('left)
  }

  "fxGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxGe.fnSimple(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxGe.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxGe.fnSimple(newCtxt) should be('left)
  }

  "fxEq" should "correctly return whether former equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxEq.fnSimple(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxEq.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxEq.fnSimple(newCtxt) should be('left)
  }

  "fxNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxNe.fnSimple(newCtxt) should be(Right(RblBool(false)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxNe.fnSimple(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxNe.fnSimple(newCtxt) should be('left)
  }

  "fxMin" should "correctly return the smallest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, Fixnum(2)), Tuple(Fixnum(5))))
    fxMin.fnSimple(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMin.fnSimple(newCtxt) should be('left)
  }

  "fxMax" should "correctly return the greatest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, Fixnum(2)), Tuple(1, Fixnum(5))))
    fxMax.fnSimple(newCtxt) should be(Right(Fixnum(5)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMax.fnSimple(newCtxt) should be('left)
  }

  "fxAbs" should "correctly return absolute value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-2)))
    fxAbs.fnSimple(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(1, Ob.NIV))
    fxAbs.fnSimple(newCtxt) should be('left)
  }

  "fxExpt" should "correctly return base-number raised to the power power-number" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxExpt.fnSimple(newCtxt) should be(Right(Fixnum(32)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxExpt.fnSimple(newCtxt) should be('left)
  }

  "fxLg" should "correctly compute Ceil(ln(input_value))" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLg.fnSimple(newCtxt) should be(Right(Fixnum(4)))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-1)))
    fxLg.fnSimple(newCtxt) should be('left)
  }

  "fxLgf" should "correctly compute Floor(ln(input_value))" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLgf.fnSimple(newCtxt) should be(Right(Fixnum(3)))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-1)))
    fxLgf.fnSimple(newCtxt) should be('left)
  }

  "fxLogand" should "correctly return the bitwise 'and' of all input values" in {
    val newCtxt =
      ctxt.copy(nargs = 3,
                argvec = Tuple(Tuple(Fixnum(8)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogand.fnSimple(newCtxt) should be(Right(Fixnum(0)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogand.fnSimple(newCtxt2) should be(Right(Fixnum(~0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogand.fnSimple(newCtxt) should be('left)
  }

  "fxLogor" should "correctly return the bitwise 'or' of all input values" in {
    val newCtxt =
      ctxt.copy(nargs = 3,
                argvec = Tuple(Tuple(Fixnum(8)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogor.fnSimple(newCtxt) should be(Right(Fixnum(14)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogor.fnSimple(newCtxt2) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogor.fnSimple(newCtxt) should be('left)
  }

  "fxLogxor" should "correctly return the bitwise 'xor' of all input values" in {
    val newCtxt =
      ctxt.copy(nargs = 3,
                argvec = Tuple(Tuple(Fixnum(6)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogxor.fnSimple(newCtxt) should be(Right(Fixnum(0)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogxor.fnSimple(newCtxt2) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogxor.fnSimple(newCtxt) should be('left)
  }

  "fxLognot" should "correctly compute bitwise '!' of input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLognot.fnSimple(newCtxt) should be(Right(Fixnum(-11)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLognot.fnSimple(newCtxt) should be('left)
  }

  "fxMdiv" should "correctly return former value module division latter value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxMdiv.fnSimple(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxMdiv.fnSimple(newCtxt) should be('left)
  }

  "fxCdiv" should "correctly compute Ceil(n/m)" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxCdiv.fnSimple(newCtxt) should be(Right(Fixnum(3)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxCdiv.fnSimple(newCtxt) should be('left)
  }

  "fxAsl" should "correctly compute arithmetical left shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxAsl.fnSimple(newCtxt) should be(Right(Fixnum(20)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxAsl.fnSimple(newCtxt) should be('left)
  }

  "fxAsr" should "correctly compute arithmetical right shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(2))))
    fxAsr.fnSimple(newCtxt) should be(Right(Fixnum(-1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxAsr.fnSimple(newCtxt) should be('left)
  }

  "fxLsl" should "correctly compute logical left shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(1))))
    fxLsl.fnSimple(newCtxt) should be(Right(Fixnum(-2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLsl.fnSimple(newCtxt) should be('left)
  }

  "fxLsr" should "correctly compute logical right shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(31))))
    fxLsr.fnSimple(newCtxt) should be(Right(Fixnum(1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLsr.fnSimple(newCtxt) should be('left)
  }
}
