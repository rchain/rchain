package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.rosette.Location._
import coop.rchain.rosette.Ob.Lenses._

class CtxtSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt.empty
    .copy(id = 0)
    .copy(ctxt = Ctxt.empty.copy(id = 1))

  "applyK" should "save result into parent ctxt" in {
    val ctxt0 = Ctxt.applyK(Fixnum(1), LocTrgt).runS(ctxt).value

    ctxt0.ctxt.trgt should be(Fixnum(1))
  }

  it should "return parent ctxt as a continuation if outstanding field of parent is one" in {
    val ctxt0                = ctxt.set(_ >> 'ctxt >> 'outstanding)(1)
    val (_, optContinuation) = Ctxt.applyK(Fixnum(1), LocTrgt).runA(ctxt0).value

    optContinuation.isDefined should be(true)
    optContinuation.get.id should be(1)
    optContinuation.get.outstanding should be(0)
  }

  "rcv" should "return itself as a continuation if outstanding field is one" in {
    val ctxt0                = ctxt.copy(outstanding = 1)
    val (_, optContinuation) = Ctxt.rcv(Fixnum(1), LocTrgt).runA(ctxt0).value

    optContinuation.isDefined should be(true)
    optContinuation.get.id should be(0)
  }

  it should "decrease the outstanding field by one if storing was successful" in {
    val ctxt0           = ctxt.copy(outstanding = 1)
    val (ctxt1, result) = Ctxt.rcv(Fixnum(1), LocTrgt).run(ctxt0).value

    ctxt1.trgt should be(Fixnum(1))
    result._1 should be(false)
    ctxt1.outstanding should be(0)
  }

  "ret" should "return false if tag is Limbo" in {
    val ctxt0  = ctxt.copy(tag = Limbo)
    val result = Ctxt.rcv(Fixnum(1), LocTrgt).runA(ctxt0).value

    result._1 should be(false)
  }

  it should "return parent ctxt as a continuation if outstanding field of parent is one" in {
    val ctxt0                = ctxt.set(_ >> 'ctxt >> 'outstanding)(1)
    val (_, optContinuation) = Ctxt.applyK(Fixnum(1), LocTrgt).runA(ctxt0).value

    optContinuation.isDefined should be(true)
    optContinuation.get.id should be(1)
    optContinuation.get.outstanding should be(0)
  }
}
