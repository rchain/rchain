package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.rosette.Location._
import coop.rchain.rosette.Ob.Lenses._

class CtxtSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt.empty
    .copy(id = 0)
    .copy(ctxt = Ctxt.empty.copy(id = 1))

  val globalEnv = TblObject(Seq())

  val initial = (globalEnv, ctxt)

  "applyK" should "save result into ctxt's continuation" in {
    val (conts, (_, ctxt0), res) = Ctxt.applyK(Fixnum(1), LocTrgt).run((), initial).value

    ctxt0.ctxt.trgt should be(Fixnum(1))
  }

  it should "return continuation if outstanding field of continuation is one" in {
    val ctxt0               = ctxt.set(_ >> 'ctxt >> 'outstanding)(1)
    val (conts, state, res) = Ctxt.applyK(Fixnum(1), LocTrgt).run((), (globalEnv, ctxt0)).value

    conts.nonEmpty should be(true)
    conts.head.id should be(1)
    conts.head.outstanding should be(0)
  }

  "rcv" should "schedule itself if outstanding field is one" in {
    val ctxt0               = ctxt.copy(outstanding = 1)
    val (conts, state, res) = Ctxt.rcv(Fixnum(1), LocTrgt).run((), (globalEnv, ctxt0)).value

    conts.nonEmpty should be(true)
    conts.head.id should be(0)
  }

  it should "decrease the outstanding field by one if storing was successful" in {
    val ctxt0                    = ctxt.copy(outstanding = 1)
    val (conts, (_, ctxt1), res) = Ctxt.rcv(Fixnum(1), LocTrgt).run((), (globalEnv, ctxt0)).value

    ctxt1.trgt should be(Fixnum(1))
    res should be(false)
    ctxt1.outstanding should be(0)
  }

  it should "return false if tag is `Limbo`" in {
    val ctxt0 = ctxt.copy(tag = Limbo)
    val res   = Ctxt.rcv(Fixnum(1), LocTrgt).runA((), (globalEnv, ctxt0)).value

    res should be(false)
  }

  "ret" should "save result to given location in its continuation" in {
    val ctxt0                    = ctxt.copy(tag = LocTrgt)
    val (conts, (_, ctxt1), res) = Ctxt.ret(Fixnum(1)).run((), (globalEnv, ctxt0)).value

    res should be(false)
    ctxt1.ctxt.trgt should be(Fixnum(1))
    conts.isEmpty should be(true)
  }

  it should "schedule continuation if outstanding field of continuation is one" in {
    val ctxt0                    = ctxt.set(_ >> 'ctxt >> 'outstanding)(1).set(_ >> 'tag)(LocTrgt)
    val (conts, (_, ctxt1), res) = Ctxt.ret(Fixnum(1)).run((), (globalEnv, ctxt0)).value

    res should be(false)
    ctxt1.ctxt.trgt should be(Fixnum(1))
    conts.nonEmpty should be(true)
    conts.head.trgt should be(Fixnum(1))
  }
}
