package coop.rchain.roscala

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.fixnum.fxPlus
import org.scalatest.{FlatSpec, Matchers}

class VmSpec extends FlatSpec with Matchers {

  val globalEnv = new GlobalEnv()

  /**
    * Add key-value pair to the parent (sbo) of all `Fixnum`s
    * where the key is the add operation and the value is the
    * primitive for addition of `Fixnum`s.
    */
  val addOprn = Oprn()
  Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, addOprn, fxPlus, ctxt = null)(globalEnv)

  /* Add `addOprn` to first position in `globalEnv` */
  globalEnv.addSlot(Symbol("+"), addOprn)

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

    rtnCtxt.rslt shouldBe Fixnum(1)
  }

  "(+ 1 (+ 2 3))" should "return 6" in {

    /**
      * litvec:
      *  0:   {RequestExpr}
      * codevec:
      *  0:   alloc 2
      *  1:   lit 1,arg[0]
      *  2:   xfer global[+],trgt
      *  4:   outstanding 12,1
      *  6:   push/alloc 2
      *  7:   lit 2,arg[0]
      *  8:   lit 3,arg[1]
      *  9:   xfer global[+],trgt
      *  11:  xmit/nxt 2,arg[1]
      *  12:  xmit/nxt 2
      */
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpOutstanding(pc = 9, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(literal = `2`, arg = 0),
      OpImmediateLitToArg(literal = `3`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(6)
  }
}
