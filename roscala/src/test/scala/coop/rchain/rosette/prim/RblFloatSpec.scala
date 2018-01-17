package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.RblFloat._
import coop.rchain.rosette.{Ctxt, Fixnum => RFixnum, Ob, PC, RblBool, RblFloat => RFloat, Tuple}
import org.scalatest._

class RblFloatSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(1, RFixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null,
  )

  "flPlus" should "correctly add float numbers" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, RFloat(.1)))
    flPlus.fn(newCtxt) should be(Right(RFloat(.5)))

    val newCtxt2 = ctxt.copy(nargs = 0, argvec = Tuple.NIL)
    flPlus.fn(newCtxt2) should be(Right(RFloat(0)))

    val newCtxt3 = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(.1)))
    flPlus.fn(newCtxt3) should be(Right(RFloat(0.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    flPlus.fn(newCtxt) should be('left)
  }

  "flMinus" should "correctly subtract RblFloat" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, RFloat(1.1)))
    flMinus.fn(newCtxt) should be(Right(RFloat(0.0)))
  }

  it should "correctly invert the RblFloat" in {
    val newCtxt2 = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(1.1)))
    flMinus.fn(newCtxt2) should be(Right(RFloat(-1.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMinus.fn(newCtxt) should be('left)
  }

  "flTimes" should "correctly multiply float number" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, RFloat(0.5)))
    flTimes.fn(newCtxt) should be(Right(RFloat(0.125)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 3, argvec = Tuple(3, Ob.NIV))
    flTimes.fn(newCtxt) should be('left)
  }

  "flDiv" should "correctly divide float numbers" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(7.5)), Tuple(RFloat(2.5))))
    flDiv.fn(newCtxt) should be(Right(RFloat(3)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flDiv.fn(newCtxt) should be('left)
  }

  "flLt" should "correctly return whether former smaller than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.1)), Tuple(RFloat(2.2))))
    flLt.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.1)), Tuple(Ob.NIV)))
    flLt.fn(newCtxt) should be('left)
  }

  "flLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, RFloat(2.1)))
    flLe.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2)), Tuple(Ob.NIV)))
    flLe.fn(newCtxt) should be('left)
  }

  "flGt" should "correctly return whether former greater than latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.3)), Tuple(RFloat(2.2))))
    flGt.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.1)), Tuple(Ob.NIV)))
    flGt.fn(newCtxt) should be('left)
  }

  "flGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.2)), Tuple(RFloat(2.2))))
    flGe.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.1)), Tuple(Ob.NIV)))
    flGe.fn(newCtxt) should be('left)
  }

  "flEq" should "correctly return whether former equal to the latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2.2)), Tuple(RFloat(2.2))))
    flEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFixnum(2)), Tuple(Ob.NIV)))
    flEq.fn(newCtxt) should be('left)
  }

  "flNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(5)), Tuple(RFloat(5))))
    flNe.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2)), Tuple(Ob.NIV)))
    flNe.fn(newCtxt) should be('left)
  }

  "flMin" should "correctly return the smallest input value" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, RFloat(2.1)), Tuple(RFloat(2.2))))
    flMin.fn(newCtxt) should be(Right(RFloat(2.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMin.fn(newCtxt) should be('left)
  }

  "flMax" should "correctly return the greatest input value" in {
    val newCtxt = ctxt.copy(nargs = 4, argvec = Tuple(Tuple(3, RFloat(2.1)), Tuple(RFloat(2.2))))
    flMax.fn(newCtxt) should be(Right(RFloat(2.2)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flMax.fn(newCtxt) should be('left)
  }

  "flAbs" should "correctly return absolute value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(-2.1)))
    flAbs.fn(newCtxt) should be(Right(RFloat(2.1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(1, Ob.NIV))
    flAbs.fn(newCtxt) should be('left)
  }

  "flExp" should "correctly return e to the power of the input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(2.5)))
    flExp.fn(newCtxt) should be(Right(RFloat(math.exp(2.5))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flExp.fn(newCtxt) should be('left)
  }

  "flExpt" should "correctly return first argument to the power of second argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(Tuple(RFloat(2)), Tuple(RFloat(.5))))
    flExpt.fn(newCtxt) should be(Right(RFloat(math.pow(2, .5))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    flExpt.fn(newCtxt) should be('left)
  }

  "flLog" should "correctly return result of natural logarithm for input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(Math.E)))
    flLog.fn(newCtxt) should be(Right(RFloat(1)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flLog.fn(newCtxt) should be('left)
  }

  "flLog10" should "correctly return result of common logarithm for input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(100.0)))
    flLog10.fn(newCtxt) should be(Right(RFloat(2.0)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flLog10.fn(newCtxt) should be('left)
  }

  "flCeil" should "correctly return ceiling of input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(2.1)))
    flCeil.fn(newCtxt) should be(Right(RFloat(3.0)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flCeil.fn(newCtxt) should be('left)
  }

  "flFloor" should "correctly return floor of input value" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(2.1)))
    flFloor.fn(newCtxt) should be(Right(RFloat(2.0)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flFloor.fn(newCtxt) should be('left)
  }

  "flAtan" should "correctly return the arc tangent of a value " in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(1.0)))
    // the returned angle is in the range -pi/2 through pi/2.
    flAtan.fn(newCtxt) should be(Right(RFloat(Math.PI / 4)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flAtan.fn(newCtxt) should be('left)
  }

  "flSin" should "correctly return the trigonometric sine of an angle." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(Math.PI)))
    flSin.fn(newCtxt) should be(Right(RFloat(Math.sin(Math.PI))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flSin.fn(newCtxt) should be('left)
  }

  "flCos" should "correctly return the trigonometric cosine of an angle." in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(Math.PI)))
    flCos.fn(newCtxt) should be(Right(RFloat(Math.cos(Math.PI))))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flFloor.fn(newCtxt) should be('left)
  }

  "flToFx" should "correctly convert the RblFloat value to Fixnum" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(RFloat(2.1)))
    flToFx.fn(newCtxt) should be(Right(RFixnum(2)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Ob.NIV))
    flToFx.fn(newCtxt) should be('left)
  }
}
