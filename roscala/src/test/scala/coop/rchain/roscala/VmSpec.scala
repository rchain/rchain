package coop.rchain.roscala

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob.{Code, Ctxt, Fixnum}
import org.scalatest.{FlatSpec, Matchers}

class VmSpec extends FlatSpec with Matchers {
  "(if #t 1 2)" should "return 1" in {

    /**
      * litvec:
      *  0:   {IfExpr}
      * codevec:
      *  0:   lit #t,rslt
      *  1:   jf 4
      *  2:   lit 1,rslt
      *  3:   rtn/nxt
      *  4:   lit 2,rslt
      *  5:   rtn/nxt
      */
    val codevec = Seq(
      OpImmediateLitToReg(literal = `#t`, reg = rslt),
      OpJmpFalse(4),
      OpImmediateLitToReg(literal = `1`, reg = rslt),
      OpRtn(next = true),
      OpImmediateLitToReg(literal = `2`, reg = rslt),
      OpRtn(next = true)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, new GlobalEnv(), Vm.State())

    ctxt.rslt should be(Fixnum(1))
  }
}
