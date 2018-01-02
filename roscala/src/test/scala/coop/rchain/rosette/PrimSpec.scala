package coop.rchain.rosette

import coop.rchain.rosette.prim.Number._
import org.scalactic.TolerantNumerics
import org.scalatest._

class PrimSpec extends FlatSpec with Matchers {
  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

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
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, Fixnum(2)), Tuple(Fixnum(5))))
    fxMin.fn(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    fxMin.fn(newCtxt) should be('left)
  }

  "fxMax" should "correctly return the greatest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, Fixnum(2)), Tuple(1, Fixnum(5))))
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

  // test cases for RblFloat
  "flPlus" should "correctly add float numbers" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, RblFloat(.1)))
    flPlus.fn(newCtxt) should be(Right(RblFloat(.5)))

    val newCtxt2 = ctxt.copy(nargs = 0, argvec = Tuple.NIL)
    flPlus.fn(newCtxt2) should be(Right(RblFloat(0)))

    val newCtxt3 = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(.1)))
    flPlus.fn(newCtxt3) should be(Right(RblFloat(.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    flPlus.fn(newCtxt) should be('left)
  }

  "flMinus" should "correctly subtract numbers or negative the number" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, RblFloat(1.1)))
    flMinus.fn(newCtxt) should be(Right(RblFloat(0.0)))

    // when there is only one argument, flMinus should -1 * input_value
    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(1.1)))
    flMinus.fn(newCtxt2) should be(Right(RblFloat(-1.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMinus.fn(newCtxt) should be('left)
  }

  "flTimes" should "correctly multiply float number" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, RblFloat(0.5)))
    flTimes.fn(newCtxt) should be(Right(RblFloat(0.125)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Ob.NIV))
    flTimes.fn(newCtxt) should be('left)
  }

  "flDiv" should "correctly divide float numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(7.5)), Tuple(RblFloat(2.5))))
    flDiv.fn(newCtxt) should be(Right(RblFloat(3)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flDiv.fn(newCtxt) should be('left)
  }

  "flLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.1)), Tuple(RblFloat(2.2))))
    flLt.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.1)), Tuple(Ob.NIV)))
    flLt.fn(newCtxt) should be('left)
  }

  "flLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, RblFloat(2.1)))
    flLe.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2)), Tuple(Ob.NIV)))
    flLe.fn(newCtxt) should be('left)
  }

  "flGt" should "correctly return whether former greater than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.3)), Tuple(RblFloat(2.2))))
   flGt.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.1)), Tuple(Ob.NIV)))
    flGt.fn(newCtxt) should be('left)
  }

  "flGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.2)), Tuple(RblFloat(2.2))))
    flGe.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.1)), Tuple(Ob.NIV)))
    flGe.fn(newCtxt) should be('left)
  }

  "flEq" should "correctly return whether former equal to the latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2.2)), Tuple(RblFloat(2.2))))
    flEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(Fixnum(2)), Tuple(Ob.NIV)))
    flEq.fn(newCtxt) should be('left)
  }

  "flNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(5)), Tuple(RblFloat(5))))
    flNe.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2)), Tuple(Ob.NIV)))
    fxNe.fn(newCtxt) should be('left)
  }

  "flMin" should "correctly return the smallest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, RblFloat(2.1)), Tuple(RblFloat(2.2))))
    flMin.fn(newCtxt) should be(Right(RblFloat(2.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMin.fn(newCtxt) should be('left)
  }

  "flMax" should "correctly return the greatest one" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, RblFloat(2.1)), Tuple(RblFloat(2.2))))
    flMax.fn(newCtxt) should be(Right(RblFloat(2.2)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMax.fn(newCtxt) should be('left)
  }

  "flAbs" should "correctly return absolute value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(-2.1)))
    flAbs.fn(newCtxt) should be(Right(RblFloat(2.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(1, Ob.NIV))
    flAbs.fn(newCtxt) should be('left)
  }

  "flExp" should "Returns Euler's number e raised to the power of input value." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(2.5)))
    flExp.fn(newCtxt) should be(Right(RblFloat(math.exp(2.5))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flExp.fn(newCtxt) should be('left)
  }

  "flExpt" should "correctly return base-number raised to the power power-number" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RblFloat(2)), Tuple(RblFloat(.5))))
    flExpt.fn(newCtxt) should be(Right(RblFloat(math.pow(2, .5))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flExpt.fn(newCtxt) should be('left)
  }

  "flLog" should "correctly Returns the natural logarithm (base e) of a float number." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(Math.E)))
    flLog.fn(newCtxt) should be(Right(RblFloat(1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flLog.fn(newCtxt) should be('left)
  }

  "flLog10" should "correctly return the base 10 logarithm of a double value." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(100.0)))
    flLog10.fn(newCtxt) should be(Right(RblFloat(2.0)))
  }

  it should "fail for  non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flLog10.fn(newCtxt) should be('left)
  }

  "flCeil" should "correctly return ceil(input_value)" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(2.1)))
    flCeil.fn(newCtxt) should be(Right(RblFloat(3.0)))
  }

  it should "fail for  non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flCeil.fn(newCtxt) should be('left)
  }

  "flFloor" should "correctly return floor(input_value)" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(2.1)))
    flFloor.fn(newCtxt) should be(Right(RblFloat(2.0)))
  }

  it should "fail for  non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flFloor.fn(newCtxt) should be('left)
  }

  "flAtan" should "correctly return the arc tangent of a value; the returned angle is in the range -pi/2 through pi/2. " in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(1.0)))
    flAtan.fn(newCtxt) should be(Right(RblFloat(Math.PI / 4)))
  }

  it should "fail for  non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flAtan.fn(newCtxt) should be('left)
  }

  "flSin" should "correctly return the trigonometric sine of an angle." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(Math.PI)))
    flSin.fn(newCtxt) should be(Right(RblFloat(Math.sin(Math.PI))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flSin.fn(newCtxt) should be('left)
  }

  "flCos" should "correctly return Returns the trigonometric cosine of an angle." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(Math.PI)))
    flCos.fn(newCtxt) should be(Right(RblFloat(Math.cos(Math.PI))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flFloor.fn(newCtxt) should be('left)
  }

  "flToFx" should "correctly convert the float value to integer" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RblFloat(2.1)))
    flToFx.fn(newCtxt) should be(Right(Fixnum(2)))
  }

  it should "fail for  non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flToFx.fn(newCtxt) should be('left)
  }
}
