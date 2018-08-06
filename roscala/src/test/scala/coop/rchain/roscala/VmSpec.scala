package coop.rchain.roscala

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob._
import coop.rchain.roscala.ob.expr.{RequestExpr, TupleExpr}
import coop.rchain.roscala.pools.StrandPoolExecutor
import coop.rchain.roscala.prim.fixnum.fxPlus
import coop.rchain.roscala.prim.rblfloat.flPlus

class VmSpec extends VmSpecUtils {

  override def runBehaviour[E: StrandPoolExecutor]: Unit = {
    val globalEnv = new GlobalEnv()

    /**
      * Add key-value pair to the parent (sbo) of all `Fixnum`s
      * where the key is the add operation and the value is the
      * primitive for addition of `Fixnum`s.
      * And do the same for `RblFloat`.
      */
    val addOprn = new Oprn
    Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, addOprn, fxPlus, ctxt = null)(globalEnv)
    RblFloat.rblFloatSbo.meta.add(RblFloat.rblFloatSbo, addOprn, flPlus, ctxt = null)(globalEnv)

    /* Add `addOprn` to first position inBlock `globalEnv` */
    globalEnv.addSlot(Symbol("+"), addOprn)

    "(if #t 1 2)" should "return 1" inMultimode {

      /**
        * litvec:
        *   0:  {IfExpr}
        * codevec:
        *   0:  lit #t,rslt
        *   1:  jf 4
        *   2:  lit 1,rslt
        *   3:  rtn/nxt
        *   4:  lit 2,rslt
        *   5:  rtn/nxt
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

      //implicit val spe: StrandPoolExecutor[E] = implicitly[StrandPoolExecutor[E]]
      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(1)
    }

    "(if #f 1 2)" should "return 2" inMultimode {

      /**
        * litvec:
        *   0:  {IfExpr}
        * codevec:
        *   0:  lit #f,rslt
        *   1:  jf 4
        *   2:  lit 1,rslt
        *   3:  rtn/nxt
        *   4:  lit 2,rslt
        *   5:  rtn/nxt
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(2)
    }

    "(+ 1 2)" should "return 3" inMultimode {
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(3)
    }

    "(block (+ 1 2) (+ 3 4))" should "return 3 or 7" inMultimode {

      /**
        * litvec:
        *   0:  {BlockExpr}
        * codevec:
        *   0:  fork 7
        *   1:  alloc 2
        *   2:  lit 1,arg[0]
        *   3:  lit 2,arg[1]
        *   4:  xfer global[+],trgt
        *   6:  xmit/nxt 2
        *   7:  alloc 2
        *   8:  lit 3,arg[0]
        *   9:  lit 4,arg[1]
        *   10: xfer global[+],trgt
        *   12: xmit/nxt 2
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt should (be(Fixnum(3)) or be(Fixnum(7)))
    }

    "(+ 1 (+ 2 3))" should "return 6" inMultimode {

      /**
        * litvec:
        *   0:   {RequestExpr}
        * codevec:
        *   0:  alloc 2
        *   1:  lit 1,arg[0]
        *   2:  xfer global[+],trgt
        *   4:  outstanding 12,1
        *   6:  push/alloc 2
        *   7:  lit 2,arg[0]
        *   8:  lit 3,arg[1]
        *   9:  xfer global[+],trgt
        *   11: xmit/nxt 2,arg[1]
        *   12: xmit/nxt 2
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(6)
    }

    "(+ 1 (+ 2 (+ 3 4)))" should "return 10" inMultimode {

      /**
        * litvec:
        *   0:   {RequestExpr}
        * codevec:
        *   0:  alloc 2
        *   1:  lit 1,arg[0]
        *   2:  xfer global[+],trgt
        *   4:  outstanding 14,1
        *   6:  push/alloc 2
        *   7:  lit 2,arg[0]
        *   8:  xfer global[+],trgt
        *   10: outstanding 13,1
        *   12: push/alloc 2
        *   13: lit 3,arg[0]
        *   14: lit 4,arg[1]
        *   15: xfer global[+],trgt
        *   17: xmit/nxt 2,arg[1]
        *   18: xmit/nxt 2,arg[1]
        *   19: xmit/nxt 2
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(10)
    }

    "(let [[x 1] [y 2]] (+ x y))" should "return 3" inMultimode {

      /**
        * litvec:
        *   0:   {LetExpr}
        *   1:   {Template}
        * codevec:
        *   0:   alloc 2
        *   1:   lit 1,arg[0]
        *   2:   lit 2,arg[1]
        *   3:   nargs 2
        *   4:   extend 1
        *   5:   alloc 2
        *   6:   xfer lex[0,0],arg[0]
        *   7:   xfer lex[0,1],arg[1]
        *   8:   xfer global[+],trgt
        *   10:  xmit/nxt 2
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(3)

    }

    "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) 5)))" should "return 5" inMultimode {

      /**
        * litvec:
        *   0:   {LetExpr}
        *   1:   {Template}
        *   2:   {Loc lex[0,0]}
        * codevec:
        *   0:   alloc 1
        *   1:   lit #t,arg[0]
        *   2:   nargs 1
        *   3:   extend 1
        *   4:   xfer lex[0,0],rslt
        *   5:   jf 10
        *   6:   lit #f,rslt
        *   7:   xfer rslt,lex[0,0]
        *   8:   xfer lex[0,0],rslt
        *   9:   jmp 4
        *   10:  lit 5,rslt
        *   11:  rtn/nxt
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(5)
    }

    "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) x)))" should "return RblFalse" inMultimode {

      /**
        * litvec:
        *   0:   {LetExpr}
        *   1:   {Template}
        *   2:   {Loc lex[0,0]}
        * codevec:
        *   0:   alloc 1
        *   1:   lit #t,arg[0]
        *   2:   nargs 1
        *   3:   extend 1
        *   4:   xfer lex[0,0],rslt
        *   5:   jf 10
        *   6:   lit #f,rslt
        *   7:   xfer rslt,lex[0,0]
        *   8:   xfer lex[0,0],rslt
        *   9:   jmp 4
        *   10:  xfer lex[0,0],rslt
        *   11:  rtn/nxt
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe RblBool(false)
    }

    "(+ 1.2 2.3)" should "return 3.5" inMultimode {

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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe RblFloat(3.5)
    }

    "(fx+ 1 (fx+ 2 3))" should "return 6" inMultimode {

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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(6)
    }

    "100" should "return 100" inMultimode {

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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(100)
    }

    "OpApplyPrimTag" should "apply primitive function and copy result to the specified register" inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (let [[x 1]] (set! x (fx+ 7 5)))
        *
        * litvec:
        *   0:	{LetExpr}
        *   1:	{Template}
        *   2:	{Loc lex[0,0]}
        * codevec:
        *   0:	alloc 1
        *   1:	lit 1,arg[0]
        *   2:	nargs 1
        *   3:	extend 1
        *   4:	alloc 2
        *   5:	lit 7,arg[0]
        *   6:	lit 5,arg[1]
        *   7:	fx+ 2,lex[0,0]
        *   9:	xfer lex[0,0],rslt
        *   10:	rtn/nxt
        */
      val codevec = Seq(
        OpAlloc(n = 1),
        OpImmediateLitToArg(literal = `1`, arg = 0),
        OpNargs(1),
        OpExtend(1),
        OpAlloc(n = 2),
        OpImmediateLitToArg(literal = `7`, arg = 0),
        OpImmediateLitToArg(literal = `5`, arg = 1),
        OpApplyPrimTag(unwind = false, next = false, nargs = 2, primNum = 232, lit = 2),
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(12)
    }

    "OpApplyCmd" should "not change ctxt after running primitive" inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (send fx+ 1)
        * litvec:
        *   0:	{SendExpr}
        * codevec:
        *   0:	alloc 1
        *   1:	lit 1,arg[0]
        *   2:	fx+ 1
        *   4:	lit #niv,rslt
        *   5:	rtn/nxt
        *
        * OpApplyCmd runs a primitive without saving its result.
        * Therefore the successful execution of a primitive through
        * OpApplyCmd should not change the ctxt object.
        */
      val codevec = Seq(
        OpAlloc(1),
        OpImmediateLitToArg(literal = `1`, arg = 0),
        OpApplyCmd(unwind = false, next = false, nargs = 1, primNum = 232),
        OpImmediateLitToReg(literal = `NIV`, reg = rslt),
        OpRtn(next = true)
      )
      val rtnCtxt = Ctxt.outstanding(1)

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, rtnCtxt, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Niv
    }

    "OpXmitReg" should "save the result of xmit function into the specified register for future use" inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (and (+ 2 2) (+ 2 1))
        *
        * litvec:
        *   0:	{IfExpr}
        * codevec:
        *   0:	outstanding 8,1
        *   2:	push/alloc 2
        *   3:	lit 2,arg[0]
        *   4:	lit 2,arg[1]
        *   5:	xfer global[+],trgt
        *   7:	xmit/nxt 2,rslt
        *   8:	jf 15
        *   9:	alloc 2
        *   10:	lit 2,arg[0]
        *   11:	lit 1,arg[1]
        *   12:	xfer global[+],trgt
        *   14:	xmit/nxt 2
        *   15:	lit #f,rslt
        *   16:	rtn/nxt
        */
      val codevec = Seq(
        OpOutstanding(pc = 6, n = 1),
        OpPushAlloc(n = 2),
        OpImmediateLitToArg(literal = `2`, arg = 0),
        OpImmediateLitToArg(literal = `2`, arg = 1),
        OpXferGlobalToReg(reg = trgt, global = 0),
        OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
        OpJmpFalse(15),
        OpAlloc(2),
        OpImmediateLitToArg(literal = `2`, arg = 0),
        OpImmediateLitToArg(literal = `1`, arg = 1),
        OpXferGlobalToReg(reg = trgt, global = 0),
        OpXmit(unwind = false, next = true, 2),
        OpImmediateLitToReg(literal = `#f`, reg = rslt),
        OpRtn(next = true)
      )

      val rtnCtxt = Ctxt.outstanding(0)

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, rtnCtxt, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))
      rtnCtxt.rslt shouldBe Fixnum(3)
    }

    "OpXmitArg" should "save the result of the xmit function into the specified argument for future use" inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (+ (+ 2 2) (+ 2 1))
        *
        * litvec:
        *   0:	{RequestExpr}
        * codevec:
        *   0:	alloc 2
        *   1:	xfer global[+],trgt
        *   3:	outstanding 18,2
        *   5:	push/alloc 2
        *   6:	lit 2,arg[0]
        *   7:	lit 1,arg[1]
        *   8:	xfer global[+],trgt
        *   10:	xmit 2,arg[1]
        *   11:	pop
        *   12:	push/alloc 2
        *   13:	lit 2,arg[0]
        *   14:	lit 2,arg[1]
        *   15:	xfer global[+],trgt
        *   17:	xmit/nxt 2,arg[0]
        *   18:	xmit/nxt 2
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(7)
    }

    "OpXmitTag" should "store the result of the xmit function into the specified location for future use" inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (let [[x 1]] (set! x (+ 7 5)))
        *
        * litvec:
        *   0:	{LetExpr}
        *   1:	{Template}
        *   2:	{Loc lex[0,0]}
        * codevec:
        *   0:	alloc 1
        *   1:	lit 1,arg[0]
        *   2:	nargs 1
        *   3:	extend 1
        *   4:	outstanding 12,1
        *   6:	push/alloc 2
        *   7:	lit 1,arg[0]
        *   8:	lit 1,arg[1]
        *   9:	xfer global[+],trgt
        *   11:	xmit/tag/nxt 2,lex[0,0]
        *   12:	xfer lex[0,0],rslt
        *   13:	rtn/nxt
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

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(12)
    }

    "OpLookupToReg" should "lookup binding inBlock continuation when compiler can't find the compile-time binding for method." inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (cat 1)
        *
        * litvec:
        *   0:	{RequestExpr}
        *   1:	'cat
        * codevec:
        *   0:	alloc 1
        *   1:	lit 1,arg[0]
        *   2:	lookup 1,trgt
        *   3:	xmit/nxt 1
        *
        *
        * this use case will go through the `absent` branch of `OpLookUpReg`, and the successful use case should be:
        *
        * (defProc (sbo& Top) (cat x) (+ x 1))
        * (free [cat] (cat 1))     // this will print out `2`
        *
        * (free [〈id〉 …] 〈body〉) ⇒ result of 〈body〉
        * The free form informs the compiler that the 〈id〉 are to be treated as free inBlock 〈body〉;
        * otherwise, the compiler will issue warnings that the 〈id〉 do not have a compile-time binding.
        * And then it will be found inBlock somewhere.
        *
        */
      val codevec = Seq(
        OpAlloc(n = 1),
        OpImmediateLitToArg(literal = `1`, arg = 0),
        OpLookupToReg(lit = 1, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 1)
      )

      val rtnCtxt = Ctxt.outstanding(1)

      val code = Code(litvec = Seq(Niv, Symbol("cat")), codevec = codevec)

      val ctxt = Ctxt(code, rtnCtxt, LocRslt)
      // todo we set RequestExpr's parent and meta to the most outer case for simplicity, actually there would be RequestExprSBO and RequestExprMeta
      val requestExpr = new RequestExpr(Seq.empty[Ob])

      // we need RBLtopenv to end the recurse `lookup`
      requestExpr.parent = RBLtopenv()
      requestExpr.meta = Meta.empty
      ctxt.selfEnv = requestExpr
      ctxt.env = new Extension()

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      // todo when `handleMissingBinding` is implemented , there should be a runTimeError to catch
      rtnCtxt.rslt shouldBe Niv
    }

    "(+ (+ 1 2) (+ 3 4) (+ 5 6))" should "return 21" inMultimode {

      /**
        * litvec:
        *   0:   {RequestExpr}
        * codevec:
        *   0:  alloc 3
        *   1:  xfer global[+],trgt
        *   3:  outstanding 25,3
        *   5:  push/alloc 2
        *   6:  lit 5,arg[0]
        *   7:  lit 6,arg[1]
        *   8:  xfer global[+],trgt
        *   10: xmit 2,arg[2]
        *   11: pop
        *   12: push/alloc 2
        *   13: lit 3,arg[0]
        *   14: lit 4,arg[1]
        *   15: xfer global[+],trgt
        *   17: xmit 2,arg[1]
        *   18: pop
        *   19: push/alloc 2
        *   20: lit 1,arg[0]
        *   21: lit 2,arg[1]
        *   22: xfer global[+],trgt
        *   24: xmit/nxt 2,arg[0]
        *   25: xmit/nxt 3
        *
        * Notice: In Roscala opcode positions are not influenced by
        * the size of an opcode. Every opcode just counts `1`.
        * Therefore we have to adjust the `outstanding` from `pc = 25`
        * to `pc = 20`.
        */
      val codevec = Seq(
        OpAlloc(3),
        OpXferGlobalToReg(global = 0, reg = trgt),
        OpOutstanding(pc = 20, n = 3),
        OpPushAlloc(2),
        OpImmediateLitToArg(literal = `5`, arg = 0),
        OpImmediateLitToArg(literal = `6`, arg = 1),
        OpXferGlobalToReg(global = 0, reg = trgt),
        OpXmitArg(unwind = false, next = false, nargs = 2, arg = 2),
        OpPop,
        OpPushAlloc(2),
        OpImmediateLitToArg(literal = `3`, arg = 0),
        OpImmediateLitToArg(literal = `4`, arg = 1),
        OpXferGlobalToReg(global = 0, reg = trgt),
        OpXmitArg(unwind = false, next = false, nargs = 2, arg = 1),
        OpPop,
        OpPushAlloc(2),
        OpImmediateLitToArg(literal = `1`, arg = 0),
        OpImmediateLitToArg(literal = `2`, arg = 1),
        OpXferGlobalToReg(global = 0, reg = trgt),
        OpXmitArg(unwind = false, next = true, nargs = 2, arg = 0),
        OpXmit(unwind = false, next = true, nargs = 3)
      )

      val rtnCtxt = Ctxt.outstanding(1)

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, rtnCtxt, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(21)
    }

    "OpLookupToArg" should "lookup binding inBlock continuation when compiler can't find the compile-time binding for method." inMultimode {

      /**
        * This opcode sequence will be generated by the RBL
        * compiler with the expression: (+ cat 1)
        *
        * litvec:
        *   0:	{RequestExpr}
        *   1:	'cat
        * codevec:
        *   0:	alloc 2
        *   1:	lookup 1,arg[0]
        *   2:	lit 1,arg[1]
        *   3:	xfer global[+],trgt
        *   5:	xmit/nxt 2
        *
        *
        * this use case will go through the `absent` branch of `OpLookUpArg`, and the successful use case should be:
        *
        * (defProc (sbo& Top) (cat x) (+ x 1))
        * (free [cat] (let [[x cat]] (x 1)))     // this will print out `2`
        *
        * (free [〈id〉 …] 〈body〉) ⇒ result of 〈body〉
        * The free form informs the compiler that the 〈id〉 are to be treated as free inBlock 〈body〉;
        * otherwise, the compiler will issue warnings that the 〈id〉 do not have a compile-time binding.
        * And then it will be found inBlock somewhere.
        *
        */
      val codevec = Seq(
        OpAlloc(n = 2),
        OpLookupToArg(lit = 1, arg = 0),
        OpImmediateLitToArg(literal = `1`, arg = 1),
        OpXferGlobalToReg(global = 0, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 2)
      )

      val rtnCtxt = Ctxt.outstanding(1)

      val code = Code(litvec = Seq(Niv, Symbol("cat")), codevec = codevec)

      val ctxt = Ctxt(code, rtnCtxt, LocRslt)
      // todo we set RequestExpr's parent and meta to the most outer case for simplicity, actually there would be RequestExprSBO and RequestExprMeta
      val requestExpr = new RequestExpr(Seq.empty[Ob])

      // we need RBLtopenv to end the recurse `lookup`
      requestExpr.parent = RBLtopenv()
      requestExpr.meta = Meta.empty
      ctxt.selfEnv = requestExpr
      ctxt.env = new Extension()

      Vm.run(ctxt, Vm.State(globalEnv = globalEnv))

      // todo when `handleMissingBinding` is implemented , there should be a runTimeError to catch
      rtnCtxt.rslt shouldBe Niv
    }
  }
}
