package coop.rchain.roscala

import coop.rchain.roscala.ob.{Ctxt, Fixnum, Ob, Tuple}
import org.scalatest.{FlatSpec, Matchers}

class CtxtSpec extends FlatSpec with Matchers {

  "clone()" should "not reference the old program counter" in {
    val ctxt = Ctxt.empty
    ctxt.pc = 1

    val clone = ctxt.clone()
    clone.pc = 2

    ctxt.pc shouldNot be(clone.pc)
  }

  "clone()" should "not reference the old argvec" in {
    val ob  = Fixnum(1)
    val ob2 = Fixnum(2)

    val ctxt = Ctxt.empty
    ctxt.argvec = Tuple(new Array[Ob](1))
    ctxt.argvec.update(0, ob)

    val clone = ctxt.clone()
    ctxt.argvec.update(0, ob2)

    ctxt.arg(0) shouldNot be(clone.arg(0))
  }
}
