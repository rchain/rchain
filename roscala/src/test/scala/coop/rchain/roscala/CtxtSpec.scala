package coop.rchain.roscala

import coop.rchain.roscala.ob.{Ctxt, Fixnum, Ob, Tuple}
import org.scalatest.{FlatSpec, Matchers}

class CtxtSpec extends FlatSpec with Matchers {

  "copy()" should "should not reference the old program counter" in {
    val ctxt = Ctxt.empty
    ctxt.pc = 1

    val copy = ctxt.copy()
    copy.pc = 2

    ctxt.pc shouldNot be(copy.pc)
  }

  "copy()" should "not reference the old argvec" in {
    val ob  = Fixnum(1)
    val ob2 = Fixnum(2)

    val ctxt = Ctxt.empty
    ctxt.argvec = new Tuple(new Array[Ob](1))
    ctxt.argvec.update(0, ob)

    val copy = ctxt.copy()
    ctxt.argvec.update(0, ob2)

    ctxt.arg(0) shouldNot be(copy.arg(0))
  }
}
