package coop.rchain.roscala

import java.util.concurrent.atomic.AtomicInteger

import coop.rchain.roscala.ob.{Invalid, _}
import org.scalatest._

class LocationSpec extends WordSpec with Matchers {
  def fixture = new {
    val fixnum8 = new Fixnum(value = 8)
    fixnum8.parent = new Fixnum(0)

    val fixnum7 = new Fixnum(7)
    fixnum7.meta = Meta.empty
    fixnum7.parent = fixnum8

    val testCtxt = new Ctxt(
      tag = CtxtRegister(0),
      nargs = 1,
      outstanding = new AtomicInteger(0),
      pc = 0,
      argvec = Tuple(Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6)),
      env = fixnum7,
      code = null,
      ctxt = None,
      self2 = null,
      selfEnv = null,
      rcvr = null,
      monitor = null
    )

    testCtxt.rslt = Fixnum(1)
    testCtxt.trgt = Fixnum(2)

    val globalEnv = new GlobalEnv()

    val initial = (globalEnv, testCtxt)
  }

  "Location.store" should {
    "store to register" in {
      // Only testing registers which accept an Ob
      val f = fixture
      import f._

      val regs = Seq(0, 1, 3, 6, 7, 8)

      regs.map { reg =>
        val res = Location.store(CtxtRegister(reg), testCtxt, RblString("test"))

        res shouldBe false
        testCtxt.reg(reg = reg) shouldBe RblString("test")
      }
    }

    "an IllegalArgumentException should be thrown for invalid register" in {
      val f = fixture
      import f._

      the[IllegalArgumentException] thrownBy {
        Location.store(CtxtRegister(100), testCtxt, RblString("test"))
      } should have message "Unknown register"
    }

    "store to argvec" in {
      val f = fixture
      import f._

      val argRegs = 0 to 3

      argRegs.map { argReg =>
        val res = Location.store(ArgRegister(argReg), testCtxt, RblString("test"))

        res shouldBe false
        testCtxt.arg(argReg) shouldBe RblString("test")
      }
    }

    "return Failure for invalid argvec position" in {
      val f = fixture
      import f._

      val res = Location.store(ArgRegister(100), testCtxt, RblString("test"))

      res shouldBe true
    }

    "store from environment" in {
      val f = fixture
      import f._

      val lexVarOffset1 = LexVariable(indirect = false, level = 0, offset = 1)
      val lexVarLevel0  = LexVariable(indirect = false, level = 0, offset = 0)
      val lexVarLevel1  = LexVariable(indirect = false, level = 1, offset = 0)

      {
        val res = Location.store(lexVarOffset1, testCtxt, RblString("test"))

        res shouldBe false
        testCtxt.env.slot(1) shouldBe Some(RblString("test"))
      }

      {
        val res = Location.store(lexVarLevel0, testCtxt, RblString("test"))

        res shouldBe false
        testCtxt.env.slot(0) shouldBe Some(RblString("test"))
      }

      {
        val res = Location.store(lexVarLevel1, testCtxt, RblString("test"))

        res shouldBe false
        testCtxt.env.parent.slot(0) shouldBe Some(RblString("test"))
      }
    }
  }

  "return Failure for invalid environment" in {
    val f = fixture
    import f._
    {
      val variable = LexVariable(indirect = false, level = 100, offset = 0)
      an[NullPointerException] should be thrownBy {
        Location.store(variable, testCtxt, RblString("test"))
      }

    }

    /** TODO: Uncomment when setLex add logic to deal with the case that `numberOfSlot` is smaller than `offset`
    {
      val variable = LexVariable(indirect = false, level = 0, offset = 100)
      val res      = Location.store(variable, testCtxt, RblString("test"))

      res shouldBe true
    }
    */
  }

  "Location.fetch" should {
    "fetch from register" in {
      val f = fixture
      import f._

      val regs = 0 to 9

      regs.map(
        reg => Location.fetch(CtxtRegister(reg), testCtxt, globalEnv) shouldBe testCtxt.reg(reg)
      )
    }

    "return None for invalid register" in {
      val f = fixture
      import f._

      Location.fetch(CtxtRegister(100), testCtxt, globalEnv) shouldBe Invalid
    }

    "fetch from argvec" in {
      val f = fixture
      import f._

      val argRegs = 0 to 3

      argRegs.map(
        argReg =>
          Location.fetch(ArgRegister(argReg), testCtxt, globalEnv) shouldBe testCtxt.arg(argReg)
      )
    }

    "return None for invalid argvec position" in {
      val f = fixture
      import f._

      Location.fetch(ArgRegister(100), testCtxt, globalEnv) shouldBe Invalid
    }

    "fetch from environment" in {
      val f = fixture
      import f._

      val lexVarOffset1 = LexVariable(indirect = false, level = 0, offset = 1)
      val lexVarOffset2 = LexVariable(indirect = false, level = 0, offset = 2)
      val lexVarLevel0  = LexVariable(indirect = false, level = 0, offset = 0)
      val lexVarLevel1  = LexVariable(indirect = false, level = 1, offset = 0)

      Location.store(lexVarOffset1, testCtxt, RblString("test"))
      Location.store(lexVarLevel0, testCtxt, Fixnum(11))
      Location.store(lexVarLevel1, testCtxt, RblString("test"))

      Location.fetch(lexVarOffset1, testCtxt, globalEnv) shouldBe RblString("test")
      Location.fetch(lexVarLevel0, testCtxt, globalEnv) shouldBe Fixnum(11)
      Location.fetch(lexVarLevel1, testCtxt, globalEnv) shouldBe RblString("test")
      Location.fetch(lexVarOffset2, testCtxt, globalEnv) shouldBe Invalid
    }
  }
}
