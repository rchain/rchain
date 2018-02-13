package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class LocationSpec extends WordSpec with PropertyChecks with Matchers {
  val testCtxt = Ctxt(
    tag = CtxtRegister(0),
    nargs = 1,
    outstanding = 0,
    pc = PC(0),
    rslt = Fixnum(1),
    trgt = Fixnum(2),
    argvec = Tuple(Seq(Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))),
    env = Fixnum(7, slot = Seq(StdMeta(), Fixnum(8))),
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  "Location.store" should {

    "store to register" in {
      // Only testing registers which accept an Ob
      val regs = Seq(0, 1, 3, 6, 7, 8)

      regs.map { reg =>
        val (ctxt, res) =
          Location
            .store(CtxtRegister(reg), RblString("test"))
            .run(testCtxt)
            .value

        res shouldBe Success
        ctxt.getReg(reg) shouldBe Some(RblString("test"))
      }
    }

    "return Failure for invalid register" in {
      val (ctxt, res) =
        Location
          .store(CtxtRegister(100), RblString("test"))
          .run(testCtxt)
          .value

      res shouldBe Failure
      ctxt shouldBe testCtxt
    }

    "store to argvec" in {
      val argRegs = 0 to 3

      argRegs.map { argReg =>
        val (ctxt, res) =
          Location
            .store(ArgRegister(argReg), RblString("test"))
            .run(testCtxt)
            .value

        res shouldBe Success
        ctxt.argvec.elem(argReg) shouldBe RblString("test")
      }
    }

    "return Failure for invalid argvec position" in {
      val (ctxt, res) =
        Location
          .store(ArgRegister(100), RblString("test"))
          .run(testCtxt)
          .value

      res shouldBe Failure
      ctxt shouldBe testCtxt
    }

    "store to environment" in {
      val lexVarOffset1 = LexVariable(indirect = false, level = 0, offset = 1)
      val lexVarLevel0  = LexVariable(indirect = false, level = 0, offset = 0)
      val lexVarLevel1  = LexVariable(indirect = false, level = 1, offset = 0)

      {
        val (ctxt, res) =
          Location.store(lexVarOffset1, RblString("test")).run(testCtxt).value

        res shouldBe Success
        ctxt.env.slot.lift(1) shouldBe Some(RblString("test"))
      }

      {
        val (ctxt, res) =
          Location.store(lexVarLevel0, RblString("test")).run(testCtxt).value

        res shouldBe Success
        ctxt.env.slot.lift(0) shouldBe Some(RblString("test"))
      }

      {
        val (ctxt, res) =
          Location.store(lexVarLevel1, RblString("test")).run(testCtxt).value

        res shouldBe Success
        ctxt.env.parent.slot.lift(0) shouldBe Some(RblString("test"))
      }
    }
  }

  "return Failure for invalid environment" in {
    {
      val (ctxt, res) =
        Location
          .store(LexVariable(indirect = false, level = 100, offset = 0), RblString("test"))
          .run(testCtxt)
          .value

      res shouldBe Failure
      ctxt shouldBe testCtxt
    }

    {
      val (ctxt, res) =
        Location
          .store(LexVariable(indirect = false, level = 0, offset = 100), RblString("test"))
          .run(testCtxt)
          .value

      res shouldBe Failure
      ctxt shouldBe testCtxt
    }
  }

  "Location.fetch" should {

    "fetch from register" in {
      val regs = 0 to 9

      regs.map(
        reg =>
          Location
            .fetch(CtxtRegister(reg), null)
            .runA(testCtxt)
            .value shouldBe testCtxt.getReg(reg))
    }

    "return None for invalid register" in {
      Location
        .fetch(CtxtRegister(100), null)
        .runA(testCtxt)
        .value shouldBe None
    }

    "fetch from argvec" in {
      val argRegs = 0 to 3

      argRegs.map(
        argReg =>
          Location
            .fetch(ArgRegister(argReg), null)
            .runA(testCtxt)
            .value shouldBe testCtxt.argvec.elem.lift(argReg)
      )
    }

    "return None for invalid argvec position" in {
      Location
        .fetch(ArgRegister(100), null)
        .runA(testCtxt)
        .value shouldBe None
    }

    /* TODO: Uncomment when getLex is implemented
    "fetch from environment" in {
      Location
        .fetch(LexVariable(indirect = false, level = 0, offset = 0), null)
        .runA(testCtxt)
        .value shouldBe testCtxt.env.slot.lift(0)

      Location
        .fetch(LexVariable(indirect = false, level = 0, offset = 1), null)
        .runA(testCtxt)
        .value shouldBe testCtxt.env.slot.lift(1)

      Location
        .fetch(LexVariable(indirect = false, level = 1, offset = 0), null)
        .runA(testCtxt)
        .value shouldBe testCtxt.env.parent.slot.lift(0)
    }
   */
  }
}
