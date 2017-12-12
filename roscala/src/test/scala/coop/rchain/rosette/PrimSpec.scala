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
}
