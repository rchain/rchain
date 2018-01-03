package coop.rchain.rosette

import coop.rchain.rosette.prim.Number._

import org.scalatest._

class PrimSpec extends FlatSpec with Matchers {
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
    fxPlus.fn(newCtxt) should be(Right(Fixnum(5)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    fxPlus.fn(newCtxt) should be('left)
  }

  "fxMinus" should "correctly subtract numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(1)))
    fxMinus.fn(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    fxMinus.fn(newCtxt) should be('left)
  }

  "fxTimes" should "correctly multiply numbers" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Fixnum(5)))
    fxTimes.fn(newCtxt) should be(Right(Fixnum(125)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Ob.NIV))
    fxTimes.fn(newCtxt) should be('left)
  }

  "fxDiv" should "correctly divide numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(5)))
    fxDiv.fn(newCtxt) should be(Right(Fixnum(1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxDiv.fn(newCtxt) should be('left)
  }

  "fxMod" should "correctly return the remainder" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Fixnum(5)))
    fxMod.fn(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMod.fn(newCtxt) should be('left)
  }

  "fxLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxLt.fn(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxLt.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLt.fn(newCtxt) should be('left)
  }

  "fxLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(2))))
    fxLe.fn(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxLe.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLe.fn(newCtxt) should be('left)
  }

  "fxGt" should "correctly return whether former greater than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxGt.fn(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxGt.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxGt.fn(newCtxt) should be('left)
  }

  "fxGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxGe.fn(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxGe.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxGe.fn(newCtxt) should be('left)
  }

  "fxEq" should "correctly return whether former equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxEq.fn(newCtxt) should be(Right(RblBool(true)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxEq.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxEq.fn(newCtxt) should be('left)
  }

  "fxNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(5))))
    fxNe.fn(newCtxt) should be(Right(RblBool(false)))

    val newCtxt2 = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    fxNe.fn(newCtxt2) should be(Right(RblBool(false)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxNe.fn(newCtxt) should be('left)
  }

  "fxMin" should "correctly return the smallest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3,Fixnum(2)), Tuple(Fixnum(5))))
    fxMin.fn(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMin.fn(newCtxt) should be('left)
  }

  "fxMax" should "correctly return the greatest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3,Fixnum(2)), Tuple(1,Fixnum(5))))
    fxMax.fn(newCtxt) should be(Right(Fixnum(5)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMax.fn(newCtxt) should be('left)
  }

  "fxAbs" should "correctly return absolute value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-2)))
    fxAbs.fn(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(1, Ob.NIV))
    fxAbs.fn(newCtxt) should be('left)
  }

  "fxExpt" should "correctly return base-number raised to the power power-number" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxExpt.fn(newCtxt) should be(Right(Fixnum(32)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxExpt.fn(newCtxt) should be('left)
  }

  "fxLg" should "correctly compute Ceil(ln(input_value))" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLg.fn(newCtxt) should be(Right(Fixnum(4)))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-1)))
    fxLg.fn(newCtxt) should be('left)
  }

  "fxLgf" should "correctly compute Floor(ln(input_value))" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLgf.fn(newCtxt) should be(Right(Fixnum(3)))
  }

  it should "fail for input value <= 0" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(-1)))
    fxLgf.fn(newCtxt) should be('left)
  }

  "fxLogand" should "correctly return the bitwise 'and' of all input values" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(Tuple(Fixnum(8)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogand.fn(newCtxt) should be(Right(Fixnum(0)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogand.fn(newCtxt2) should be(Right(Fixnum(~0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogand.fn(newCtxt) should be('left)
  }

  "fxLogor" should "correctly return the bitwise 'or' of all input values" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(Tuple(Fixnum(8)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogor.fn(newCtxt) should be(Right(Fixnum(14)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogor.fn(newCtxt2) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogor.fn(newCtxt) should be('left)
  }

  "fxLogxor" should "correctly return the bitwise 'xor' of all input values" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(Tuple(Fixnum(6)), Tuple(Tuple(Fixnum(4)), Tuple(Fixnum(2)))))
    fxLogxor.fn(newCtxt) should be(Right(Fixnum(0)))

    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple.NIL)
    fxLogxor.fn(newCtxt2) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxLogxor.fn(newCtxt) should be('left)
  }

  "fxLognot" should "correctly compute bitwise '!' of input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Fixnum(10)))
    fxLognot.fn(newCtxt) should be(Right(Fixnum(-11)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLognot.fn(newCtxt) should be('left)
  }

  "fxMdiv" should "correctly return former value module division latter value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Fixnum(5))))
    fxMdiv.fn(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxMdiv.fn(newCtxt) should be('left)
  }

  "fxCdiv" should "correctly compute Ceil(n/m)" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxCdiv.fn(newCtxt) should be(Right(Fixnum(3)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxCdiv.fn(newCtxt) should be('left)
  }

  "fxAsl" should "correctly compute arithmetical left shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(5)), Tuple(Fixnum(2))))
    fxAsl.fn(newCtxt) should be(Right(Fixnum(20)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxAsl.fn(newCtxt) should be('left)
  }

  "fxAsr" should "correctly compute arithmetical right shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(2))))
    fxAsr.fn(newCtxt) should be(Right(Fixnum(-1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxAsr.fn(newCtxt) should be('left)
  }

  "fxLsl" should "correctly compute logical left shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(1))))
    fxLsl.fn(newCtxt) should be(Right(Fixnum(-2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLsl.fn(newCtxt) should be('left)
  }

  "fxLsr" should "correctly compute logical right shift of input value" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(-1)), Tuple(Fixnum(31))))
    fxLsr.fn(newCtxt) should be(Right(Fixnum(1)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    fxLsr.fn(newCtxt) should be('left)
  }
}
