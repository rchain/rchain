package coop.rchain.roscala

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob._
import coop.rchain.roscala.ob.expr.TupleExpr
import coop.rchain.roscala.prim.fixnum.fxPlus
import coop.rchain.roscala.prim.rblfloat.flPlus
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class VmSpec extends FlatSpec with Matchers {

  val globalEnv = new GlobalEnv()

  /**
    * Add key-value pair to the parent (sbo) of all `Fixnum`s
    * where the key is the add operation and the value is the
    * primitive for addition of `Fixnum`s.
    * And do the same for `RblFloat`.
    */
  val addOprn = Oprn()
  Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, addOprn, fxPlus, ctxt = null)(globalEnv)
  RblFloat.rblFloatSbo.meta.add(RblFloat.rblFloatSbo, addOprn, flPlus, ctxt = null)(globalEnv)

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

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(1)
  }

  "(if #f 1 2)" should "return 2" in {

    /**
      * litvec:
      *  0:   {IfExpr}
      * codevec:
      *  0:   lit #f,rslt
      *  1:   jf 4
      *  2:   lit 1,rslt
      *  3:   rtn/nxt
      *  4:   lit 2,rslt
      *  5:   rtn/nxt
      */
    val codevec = Seq(
      OpImmediateLitToReg(literal = `#f`, reg = rslt),
      OpJmpFalse(4),
      OpImmediateLitToReg(literal = `1`, reg = rslt),
      OpRtn(next = true),
      OpImmediateLitToReg(literal = `2`, reg = rslt),
      OpRtn(next = true)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(2)
  }

  "(+ 1 2)" should "return 3" in {
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpImmediateLitToArg(literal = `2`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmit(unwind = false, next = true, 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(3)
  }

  "(block (+ 1 2) (+ 3 4))" should "return 3 or 7" in {

    /**
      * litvec:
      *  0:   {BlockExpr}
      * codevec:
      *  0:   fork 7
      *  1:   alloc 2
      *  2:   lit 1,arg[0]
      *  3:   lit 2,arg[1]
      *  4:   xfer global[+],trgt
      *  6:   xmit/nxt 2
      *  7:   alloc 2
      *  8:   lit 3,arg[0]
      *  9:   lit 4,arg[1]
      *  10:  xfer global[+],trgt
      *  12:  xmit/nxt 2
      */
    val codevec = Seq(
      OpFork(6),
      OpAlloc(2),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpImmediateLitToArg(literal = `2`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmit(unwind = false, next = true, 2),
      OpAlloc(2),
      OpImmediateLitToArg(literal = `3`, arg = 0),
      OpImmediateLitToArg(literal = `4`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmit(unwind = false, next = true, 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt should (be(Fixnum(3)) or be(Fixnum(7)))
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

  "(+ 1 (+ 2 (+ 3 4)))" should "return 10" in {

    /**
      * litvec:
      *  0:   {RequestExpr}
      * codevec:
      *  0:   alloc 2
      *  1:   lit 1,arg[0]
      *  2:   xfer global[+],trgt
      *  4:   outstanding 14,1
      *  6:   push/alloc 2
      *  7:   lit 2,arg[0]
      *  8:   xfer global[+],trgt
      * 10:   outstanding 13,1
      * 12:   push/alloc 2
      * 13:   lit 3,arg[0]
      * 14:   lit 4,arg[1]
      * 15:   xfer global[+],trgt
      * 17:   xmit/nxt 2,arg[1]
      * 18:   xmit/nxt 2,arg[1]
      * 19:   xmit/nxt 2
      */
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpOutstanding(pc = 14, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(literal = `2`, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpOutstanding(pc = 13, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(literal = `3`, arg = 0),
      OpImmediateLitToArg(literal = `4`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(10)
  }

  "(let [[x 1] [y 2]] (+ x y))" should "return 3" in {

    /**
      * litvec:
      *  0:   {LetExpr}
      *  1:   {Template}
      * codevec:
      *  0:   alloc 2
      *  1:   lit 1,arg[0]
      *  2:   lit 2,arg[1]
      *  3:   nargs 2
      *  4:   extend 1
      *  5:   alloc 2
      *  6:   xfer lex[0,0],arg[0]
      *  7:   xfer lex[0,1],arg[1]
      *  8:   xfer global[+],trgt
      *  10:  xmit/nxt 2
      */
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpImmediateLitToArg(literal = `2`, arg = 1),
      OpNargs(2),
      OpExtend(1),
      OpAlloc(2),
      OpXferLexToArg(indirect = false, level = 0, offset = 0, arg = 0),
      OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmit(unwind = false, next = true, 2)
    )

    val keyMeta = Meta(extensible = false)
    keyMeta.map.update(Symbol("x"), LexVariable(0, 0, indirect = false))
    keyMeta.map.update(Symbol("y"), LexVariable(0, 1, indirect = false))

    val template = new Template(
      keyTuple = Tuple(Symbol("x"), Symbol("y")),
      pat = new IdVecPattern(new TupleExpr(Seq(Symbol("x"), Symbol("y")))),
      keyMeta = keyMeta
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Niv, template), codevec = codevec)

    val ctxt = Ctxt(code, rtnCtxt, LocRslt)
    ctxt.env = new Extension()

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(3)

  }

  "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) 5)))" should "return 5" in {

    /**
      * litvec:
      *    0:   {LetExpr}
      *    1:   {Template}
      *    2:   {Loc lex[0,0]}
      * codevec:
      *    0:   alloc 1
      *    1:   lit #t,arg[0]
      *    2:   nargs 1
      *    3:   extend 1
      *    4:   xfer lex[0,0],rslt
      *    5:   jf 10
      *    6:   lit #f,rslt
      *    7:   xfer rslt,lex[0,0]
      *    8:   xfer lex[0,0],rslt
      *    9:   jmp 4
      *   10:   lit 5,rslt
      *   11:   rtn/nxt
      */
    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(literal = `#t`, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmpFalse(10),
      OpImmediateLitToReg(literal = `#f`, reg = rslt),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmp(4),
      OpImmediateLitToReg(literal = `5`, reg = rslt),
      OpRtn(next = true)
    )

    val keyMeta = Meta(extensible = false)
    keyMeta.map(Symbol("x")) = LexVariable(0, 0, indirect = false)

    val template = new Template(
      keyTuple = Tuple(Symbol("x")),
      pat = new IdVecPattern(new TupleExpr(Seq(Symbol("x")))),
      keyMeta = keyMeta
    )

    val lexVar = LexVariable(0, 0, indirect = false)

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Niv, template, lexVar), codevec = codevec)

    val ctxt = Ctxt(code, rtnCtxt, LocRslt)
    ctxt.env = new Extension()

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(5)
  }

  "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) x)))" should "return RblFalse" in {

    /**
      * litvec:
      *    0:   {LetExpr}
      *    1:   {Template}
      *    2:   {Loc lex[0,0]}
      * codevec:
      *    0:   alloc 1
      *    1:   lit #t,arg[0]
      *    2:   nargs 1
      *    3:   extend 1
      *    4:   xfer lex[0,0],rslt
      *    5:   jf 10
      *    6:   lit #f,rslt
      *    7:   xfer rslt,lex[0,0]
      *    8:   xfer lex[0,0],rslt
      *    9:   jmp 4
      *   10:   xfer lex[0,0],rslt
      *   11:   rtn/nxt
      */
    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(literal = `#t`, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmpFalse(10),
      OpImmediateLitToReg(literal = `#f`, reg = rslt),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmp(4),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpRtn(next = true)
    )

    val keyMeta = Meta(extensible = false)
    keyMeta.map.update(Symbol("x"), LexVariable(0, 0, indirect = false))

    val template = new Template(
      keyTuple = Tuple(Symbol("x")),
      pat = new IdVecPattern(new TupleExpr(Seq(Symbol("x")))),
      keyMeta = keyMeta
    )

    val lexVar = LexVariable(0, 0, indirect = false)

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Niv, template, lexVar), codevec = codevec)

    val ctxt = Ctxt(code, rtnCtxt, LocRslt)
    ctxt.env = new Extension()

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe RblFalse
  }

  "(+ 1.2 2.3)" should "return 3.5" in {

    /**
      * litvec:
      *   0:   {RequestExpr}
      *   1:   1.1
      *   2:   2.2
      * codevec:
      *   0:   alloc 2
      *   1:   liti 1,arg[0]
      *   2:   liti 2,arg[1]
      *   3:   xfer global[+],trgt
      *   5:   xmit/nxt 2
      */
    val codevec = Seq(
      OpAlloc(2),
      OpIndLitToArg(lit = 1, arg = 0),
      OpIndLitToArg(lit = 2, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Niv, RblFloat(1.2), RblFloat(2.3)), codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe RblFloat(3.5)
  }

  "(fx+ 1 (fx+ 2 3))" should "return 6" in {

    /**
      * litvec:
      *   0:   {RequestExpr}
      * codevec:
      *   0:   alloc 2
      *   1:   lit 2,arg[0]
      *   2:   lit 3,arg[1]
      *   3:   fx+ 2,arg[1]
      *   5:   lit 1,arg[0]
      *   6:   fx+ 2,rslt
      *   8:   rtn/nxt
      */
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(literal = `2`, arg = 0),
      OpImmediateLitToArg(literal = `3`, arg = 1),
      OpApplyPrimArg(unwind = false, next = false, nargs = 2, primNum = 232, arg = 1), // fx+
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpApplyPrimReg(unwind = false, next = false, nargs = 2, primNum = 232, reg = rslt), // fx+
      OpRtn(next = true)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(6)
  }

  "100" should "return 100" in {

    /**
      * litvec:
      *   0:   100
      * codevec:
      *   0:   liti 0,rslt
      *   1:   rtn/nxt
      */
    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtn(next = true)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Fixnum(100)), codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(100)
  }

  "OpXmitArg" should "save the result of the xmit function into the specified argument for future use" in {

    /**
      * This opcode sequence will be generated by the RBL
      * compiler with the expression: (+ (+ 2 2) (+ 2 1))
      *
      * litvec:
      * 0:	{RequestExpr}
      * codevec:
      * 0:	alloc 2
      * 1:	xfer global[+],trgt
      * 3:	outstanding 18,2
      * 5:	push/alloc 2
      * 6:	lit 2,arg[0]
      * 7:	lit 1,arg[1]
      * 8:	xfer global[+],trgt
      * 10:	xmit 2,arg[1]
      * 11:	pop
      * 12:	push/alloc 2
      * 13:	lit 2,arg[0]
      * 14:	lit 2,arg[1]
      * 15:	xfer global[+],trgt
      * 17:	xmit/nxt 2,arg[0]
      * 18:	xmit/nxt 2
      */
    val codevec = Seq(
      OpAlloc(n = 2),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpOutstanding(pc = 14, n = 2),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(literal = `2`, arg = 0),
      OpImmediateLitToArg(literal = `1`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmitArg(unwind = false, next = false, nargs = 2, arg = 1),
      OpPop,
      OpPushAlloc(2),
      OpImmediateLitToArg(literal = `2`, arg = 0),
      OpImmediateLitToArg(literal = `2`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 0),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(7)
  }

  "OpXmitTag" should "store the result of the xmit function into the specified location for future use" in {

    /**
      * This opcode sequence will be generated by the RBL
      * compiler with the expression: (let [[x 1]] (set! x (+ 7 5)))
      *
      * litvec:
      * 0:	{LetExpr}
      * 1:	{Template}
      * 2:	{Loc lex[0,0]}
      * codevec:
      * 0:	alloc 1
      * 1:	lit 1,arg[0]
      * 2:	nargs 1
      * 3:	extend 1
      * 4:	outstanding 12,1
      * 6:	push/alloc 2
      * 7:	lit 1,arg[0]
      * 8:	lit 1,arg[1]
      * 9:	xfer global[+],trgt
      * 11:	xmit/tag/nxt 2,lex[0,0]
      * 12:	xfer lex[0,0],rslt
      * 13:	rtn/nxt
      *
      */
    val codevec = Seq(
      OpAlloc(n = 1),
      OpImmediateLitToArg(literal = `1`, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpOutstanding(pc = 10, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(literal = `7`, arg = 0),
      OpImmediateLitToArg(literal = `5`, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 0),
      OpXmitTag(unwind = false, next = true, nargs = 2, lit = 2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpRtn(next = true)
    )

    val keyMeta = Meta(extensible = false)
    keyMeta.map.update(Symbol("x"), LexVariable(0, 0, indirect = false))

    val template = new Template(
      keyTuple = Tuple(Symbol("x")),
      pat = new IdVecPattern(new TupleExpr(Seq(Symbol("x")))),
      keyMeta = keyMeta
    )

    val lexVar = LexVariable(0, 0, indirect = false)

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq(Niv, template, lexVar), codevec = codevec)

    val ctxt = Ctxt(code, rtnCtxt, LocRslt)
    ctxt.env = new Extension()

    Vm.run(ctxt, globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(12)
  }
}
