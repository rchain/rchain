package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta
import coop.rchain.rosette.expr.{LetExpr, RequestExpr, TupleExpr}
import coop.rchain.rosette.Location._
import org.scalatest._

/**
  * NOTE that in Roscala we are using one-byte offset for every opcode,
  * so PC will change per unit for every opcode
  *
  */
class TransitionSpec extends FlatSpec with Matchers {
  val testCtxt = Ctxt(
    tag = LocationGT(Location.LTCtxtRegister(0)),
    nargs = 1,
    outstanding = 0,
    pc = PC(0),
    rslt = null,
    trgt = null,
    argvec = Tuple(Ob.NIV),
    env = StdExtension(null, null),
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  val testState = VMState(
    bytecodes = Map.empty,
    code = Code(Tuple.NIL, Seq()),
    ctxt = testCtxt,
    loc = Location.LocTrgt,
    pc = PC(0),
    strandPool = Seq(),
    sleeperPool = Seq()
  )

  val stdOprnPlus = StdOprn(null)

  val stdMeta = StdMeta()

  val globalEnv = Seq.fill(669)(Ob.NIV).updated(668, stdOprnPlus)

  "Executing bytecode from expression \"(if #t 1 2)\"" should "result in state.ctxt.rslt == Fixnum(1)" in {

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
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'pc)(PC(6)) // Setting pc to 6 so that the VM halts after strand is installed

    val codevec = Seq(OpImmediateLitToReg(lit = 8, reg = 0),
                      OpJmpFalse(4),
                      OpImmediateLitToReg(lit = 1, reg = 0),
                      OpRtn(next = true),
                      OpImmediateLitToReg(lit = 2, reg = 0),
                      OpRtn(next = true))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(1)
  }

  "Executing bytecode from expression \"(if #f 1 2)\"" should "result in state.ctxt.rslt == Fixnum(2)" in {

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
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'pc)(PC(6)) // Setting pc to 6 so that the VM halts after strand is installed

    val codevec = Seq(OpImmediateLitToReg(lit = 9, reg = 0),
                      OpJmpFalse(4),
                      OpImmediateLitToReg(lit = 1, reg = 0),
                      OpRtn(next = true),
                      OpImmediateLitToReg(lit = 2, reg = 0),
                      OpRtn(next = true))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(2)
  }

  "Executing bytecode from expression \"(+ 1 2)\"" should "result in Fixnum(3)" in {

    /**
      * litvec:
      *  0:   {RequestExpr}
      * codevec:
      *  0:   alloc 2
      *  1:   lit 1,arg[0]
      *  2:   lit 2,arg[1]
      *  3:   xfer global[+],trgt
      *  5:   xmit/nxt 2
      */
    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(OpAlloc(2),
                      OpImmediateLitToArg(value = 1, arg = 0),
                      OpImmediateLitToArg(value = 2, arg = 1),
                      OpXferGlobalToReg(reg = 1, global = 668),
                      OpXmit(unwind = false, next = true, 2))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(3)
  }

  "Executing bytecode from expression \"(+ 1 (+ 2 3))\"" should "result in Fixnum(6)" in {

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
    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpOutstanding(pc = 9, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 3, arg = 1),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(6)
  }

  "Executing bytecode from expression \"(+ 1 (+ 2 (+ 3 4))\"" should "result in Fixnum(10)" in {

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
    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpOutstanding(pc = 14, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpOutstanding(pc = 13, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 3, arg = 0),
      OpImmediateLitToArg(value = 4, arg = 1),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(10)
  }

  "Executing bytecode from expression \"(let [[x 1] [y 2]] (+ x y))\"" should "result in Fixnum(3)" in {

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
    val template = Template(
      Tuple(Seq(Symbol("x"), Symbol("y"))),
      StdMeta(),
      IdVecPattern(TupleExpr(Seq(Symbol("x"), Symbol("y"))))
    )

    val start =
      testState
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(LetExpr(), template)))
        .set(_ >> 'globalEnv)(TblObject(globalEnv, null))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpNargs(2),
      OpExtend(1),
      OpAlloc(2),
      OpXferLexToArg(indirect = false, level = 0, offset = 0, arg = 0),
      OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 1),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmit(unwind = false, next = true, 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(3)
  }

  "Executing bytecode from expression \"(block (+ 1 2) (+ 3 4))\"" should "be Fixnum(3) or Fixnum(7)" in {

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
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val codevec = Seq(
      OpFork(6),
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmit(unwind = false, next = true, 2),
      OpAlloc(2),
      OpImmediateLitToArg(value = 3, arg = 0),
      OpImmediateLitToArg(value = 4, arg = 1),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmit(unwind = false, next = true, 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt should (be(Fixnum(3)) or be(Fixnum(7)))
  }

  "Executing bytecode from expression \"(+ 1.2 2.3)\"" should "result in RoFloat(3.5)" in {

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
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'code >> 'litvec)(
          Tuple(Seq(RequestExpr, RblFloat(1.2), RblFloat(2.3))))

    val codevec = Seq(
      OpAlloc(2),
      OpIndLitToArg(arg = 0, lit = 1),
      OpIndLitToArg(arg = 1, lit = 2),
      OpXferGlobalToReg(reg = 1, global = 668),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe RblFloat(3.5)
  }

  "Executing bytecode from expression \"100\"" should "result in Fixnum(100)" in {

    /**
      * litvec:
      *   0:   100
      * codevec:
      *   0:   liti 0,rslt
      *   1:   rtn/nxt
      */
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(Fixnum(100))))

    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtn(next = true)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(100)
  }

  "Executing bytecode from expression \"(100)\"" should "result in Fixnum(100)" in {

    /**
      * litvec:
      *   0:   {RequestExpr}
      *   1:   100
      * codevec:
      *   0:   liti 1,trgt
      *   1:   xmit/nxt 0
      */
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(RequestExpr, Fixnum(100))))

    val codevec = Seq(
      OpIndLitToReg(reg = 1, lit = 1),
      OpXmit(unwind = false, next = true, nargs = 0)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.trgt shouldBe Fixnum(100)
  }

  """Executing bytecode from expression "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) 5)))"""" should "result in Fixnum(5)" in {

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
    val template = Template(
      Tuple(Seq(Symbol("x"))),
      StdMeta(),
      IdVecPattern(TupleExpr(Seq(Symbol("x"))))
    )

    val loc: Location = Location.LexVar(0, 0, indirect = false)

    val start =
      testState
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(LetExpr(), template)))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'loc)(loc)

    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(value = 8, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = 0),
      OpJmpFalse(10),
      OpImmediateLitToReg(lit = 9, reg = 0),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = 0),
      OpJmp(4),
      OpImmediateLitToReg(lit = 5, reg = 0),
      OpRtn(next = true)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(5)
  }

  """Executing bytecode from expression (let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) x)))""" should "result in RBLFALSE" in {

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
    val template = Template(
      Tuple(Seq(Symbol("x"))),
      StdMeta(),
      IdVecPattern(TupleExpr(Seq(Symbol("x"))))
    )

    val loc: Location = Location.LexVar(0, 0, indirect = false)

    val start =
      testState
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(LetExpr(), template)))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'loc)(loc)

    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(value = 8, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = 0),
      OpJmpFalse(10),
      OpImmediateLitToReg(lit = 9, reg = 0),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = 0),
      OpJmp(4),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = 0),
      OpRtn(next = true)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Ob.RBLFALSE
  }

  "OpRtnReg" should "copy ctxt.rslt to given register" in {

    /**
      * This opcode sequence will never be generated by the RBL
      * compiler as it is the responsibility of the caller to
      * specify where the result will be stored.
      *
      * litvec:
      *   0:   100
      * codevec:
      *   0:   liti 0, rslt
      *   1:   rtn/reg/nxt, trgt
      */
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'code >> 'litvec)(Tuple(Seq(Fixnum(100))))

    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtnReg(next = true, reg = 1)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.trgt shouldBe Fixnum(100)
  }
}
