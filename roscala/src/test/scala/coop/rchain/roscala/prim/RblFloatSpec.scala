package coop.rchain.roscala.prim

import java.util.concurrent.atomic.AtomicInteger

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.rblfloat._
import org.scalatest._

class RblFloatSpec extends FlatSpec with Matchers {
  val globalEnv = new GlobalEnv()
  val ctxt = new Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0, //new AtomicInteger(0),
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

  "flPlus" should "correctly add float numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, RblFloat(.1)))
    flPlus.fn(newCtxt, globalEnv) should be(RblFloat(.5))

    val newCtxt2 = reAssignCtxtArgs(ctxt, 0, Nil)
    flPlus.fn(newCtxt2, globalEnv) should be(RblFloat(0))

    val newCtxt3 = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(.1)))
    flPlus.fn(newCtxt3, globalEnv) should be(RblFloat(0.1))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    flPlus.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flMinus" should "correctly subtract RblFloat" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, RblFloat(1.1)))
    flMinus.fn(newCtxt, globalEnv) should be(RblFloat(0.0))
  }

  it should "correctly invert the RblFloat" in {
    val newCtxt2 = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(1.1)))
    flMinus.fn(newCtxt2, globalEnv) should be(RblFloat(-1.1))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    flMinus.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flTimes" should "correctly multiply float number" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, RblFloat(0.5)))
    flTimes.fn(newCtxt, globalEnv) should be(RblFloat(0.125))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(3, Niv))
    flTimes.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flDiv" should "correctly divide float numbers" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(7.5), RblFloat(2.5)))
    flDiv.fn(newCtxt, globalEnv) should be(RblFloat(3))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    flDiv.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flL(t" should "correctly return whether former smaller than latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.1), RblFloat(2.2)))
    flLt.fn(newCtxt, globalEnv) should be(RblBool(true))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.1), Niv))
    flLt.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flLe" should "correctly return whether former smaller than or equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, RblFloat(2.1)))
    flLe.fn(newCtxt, globalEnv) should be(RblBool(true))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2), Niv))
    flLe.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flGt" should "correctly return whether former greater than latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.3), RblFloat(2.2)))
    flGt.fn(newCtxt, globalEnv) should be(RblBool(true))
  }

  it should "fail for non-fixnum arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.1), Niv))
    flGt.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flGe" should "correctly return whether former greater than or equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.2), RblFloat(2.2)))
    flGe.fn(newCtxt, globalEnv) should be(RblBool(true))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.1), Niv))
    flGe.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flEq" should "correctly return whether former equal to the latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2.2), RblFloat(2.2)))
    flEq.fn(newCtxt, globalEnv) should be(RblBool(true))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(2), Niv))
    flEq.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flNe" should "correctly return whether former is not equal to latter" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(5), RblFloat(5)))
    flNe.fn(newCtxt, globalEnv) should be(RblBool(false))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2), Niv))
    flNe.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flMin" should "correctly return the smallest input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(RblFloat(2.1), RblFloat(2.1), RblFloat(2.2)))
    flMin.fn(newCtxt, globalEnv) should be(RblFloat(2.1))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    flMin.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flMax" should "correctly return the greatest input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(RblFloat(2.1), RblFloat(2.1), RblFloat(2.2)))
    flMax.fn(newCtxt, globalEnv) should be(RblFloat(2.2))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    flMax.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flAbs" should "correctly return absolute value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(-2.1)))
    flAbs.fn(newCtxt, globalEnv) should be(RblFloat(2.1))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(1, Niv))
    flAbs.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flExp" should "correctly return e to the power of the input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(2.5)))
    flExp.fn(newCtxt, globalEnv) should be(RblFloat(math.exp(2.5)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flExp.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flExpt" should "correctly return first argument to the power of second argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblFloat(2), RblFloat(.5)))
    flExpt.fn(newCtxt, globalEnv) should be(RblFloat(math.pow(2, .5)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    flExpt.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flLog" should "correctly return result of natural logarithm for input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(Math.E)))
    flLog.fn(newCtxt, globalEnv) should be(RblFloat(1))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flLog.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flLog10" should "correctly return result of common logarithm for input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(100.0)))
    flLog10.fn(newCtxt, globalEnv) should be(RblFloat(2.0))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flLog10.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flCeil" should "correctly return ceiling of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(2.1)))
    flCeil.fn(newCtxt, globalEnv) should be(RblFloat(3.0))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flCeil.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flFloor" should "correctly return floor of input value" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(2.1)))
    flFloor.fn(newCtxt, globalEnv) should be(RblFloat(2.0))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flFloor.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flAtan" should "correctly return the arc tangent of a value " in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(1.0)))
    // the returned angle is in the range -pi/2 through pi/2.
    flAtan.fn(newCtxt, globalEnv) should be(RblFloat(Math.PI / 4))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flAtan.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flSin" should "correctly return the trigonometric sine of an angle." in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(Math.PI)))
    flSin.fn(newCtxt, globalEnv) should be(RblFloat(Math.sin(Math.PI)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flSin.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flCos" should "correctly return the trigonometric cosine of an angle." in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(Math.PI)))
    flCos.fn(newCtxt, globalEnv) should be(RblFloat(Math.cos(Math.PI)))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flFloor.fn(newCtxt, globalEnv) should be(Deadthread)
  }

  "flToFx" should "correctly convert the RblFloat value to Fixnum" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(RblFloat(2.1)))
    flToFx.fn(newCtxt, globalEnv) should be(Fixnum(2))
  }

  it should "fail for non-RblFloat arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Niv))
    flToFx.fn(newCtxt, globalEnv) should be(Deadthread)
  }
}
