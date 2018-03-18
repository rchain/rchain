package coop.rchain.rosette

import cats.data.State
import coop.rchain.rosette.CtxtRegName._
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
    tag = CtxtRegister(0),
    nargs = 1,
    outstanding = 0,
    pc = PC(0),
    rslt = null,
    trgt = null,
    argvec = Tuple(Seq(Ob.NIV, Ob.NIV, Ob.NIV)),
    env = StdExtension(null, null),
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  val haltCtxt = Ctxt.empty.copy(code = Code(Tuple.NIL, Seq(OpHalt())))

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
    val codevec = Seq(
      OpImmediateLitToReg(lit = 8, reg = rslt),
      OpJmpFalse(4),
      OpImmediateLitToReg(lit = 1, reg = rslt),
      OpRtn(next = true),
      OpImmediateLitToReg(lit = 2, reg = rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(1)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpImmediateLitToReg(lit = 9, reg = rslt),
      OpJmpFalse(4),
      OpImmediateLitToReg(lit = 1, reg = rslt),
      OpRtn(next = true),
      OpImmediateLitToReg(lit = 2, reg = rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(2)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, 2)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(3)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpOutstanding(pc = 9, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 3, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val code = Code(Tuple.NIL, codevec)

    val start =
      testState
        .set(_ >> 'code)(code)
        .set(_ >> 'ctxt)(testCtxt)
        .set(_ >> 'ctxt >> 'code)(code)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'id)(2)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(6)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpOutstanding(pc = 14, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpOutstanding(pc = 13, n = 1),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 3, arg = 0),
      OpImmediateLitToArg(value = 4, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val code = Code(Tuple.NIL, codevec)

    val start =
      testState
        .set(_ >> 'code)(code)
        .set(_ >> 'ctxt)(testCtxt)
        .set(_ >> 'ctxt >> 'code)(code)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(10)
    end.ctxt.id shouldBe 1
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

    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpNargs(2),
      OpExtend(1),
      OpAlloc(2),
      OpXferLexToArg(indirect = false, level = 0, offset = 0, arg = 0),
      OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, 2)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(LetExpr(), template)), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(3)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpFork(6),
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, 2),
      OpAlloc(2),
      OpImmediateLitToArg(value = 3, arg = 0),
      OpImmediateLitToArg(value = 4, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, 2)
    )

    val code = Code(Tuple.NIL, codevec)

    val start =
      testState
        .set(_ >> 'code)(code)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'code)(code)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt should (be(Fixnum(3)) or be(Fixnum(7)))
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpAlloc(2),
      OpIndLitToArg(arg = 0, lit = 1),
      OpIndLitToArg(arg = 1, lit = 2),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(RequestExpr, RblFloat(1.2), RblFloat(2.3))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe RblFloat(3.5)
    end.ctxt.id shouldBe 1
  }

  "Executing bytecode from expression \"100\"" should "result in Fixnum(100)" in {

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

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(Fixnum(100))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(100)
    end.ctxt.id shouldBe 1
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
    val codevec = Seq(
      OpIndLitToReg(reg = trgt, lit = 1),
      OpXmit(unwind = false, next = true, nargs = 0)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(RequestExpr, Fixnum(100))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

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

    val lexVar: Location = LexVariable(indirect = false, 0, 0)

    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(value = 8, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmpFalse(10),
      OpImmediateLitToReg(lit = 9, reg = rslt),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmp(4),
      OpImmediateLitToReg(lit = 5, reg = rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(LetExpr(), template, lexVar)), codevec))
        .set(_ >> 'loc)(lexVar)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(5)
    end.ctxt.id shouldBe 1
  }

  """Executing bytecode from expression "(let [[x #t]] (label l (if x (seq (set! x #f) (goto l)) x)))"""" should "result in RBLFALSE" in {

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

    val lexVar: Location = LexVariable(indirect = false, 0, 0)

    val codevec = Seq(
      OpAlloc(1),
      OpImmediateLitToArg(value = 8, arg = 0),
      OpNargs(1),
      OpExtend(1),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmpFalse(10),
      OpImmediateLitToReg(lit = 9, reg = rslt),
      OpXferRsltToDest(2),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpJmp(4),
      OpXferLexToReg(indirect = false, level = 0, offset = 0, reg = rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(LetExpr(), template, lexVar)), codevec))
        .set(_ >> 'loc)(lexVar)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Ob.RBLFALSE
    end.ctxt.id shouldBe 1
  }

  """Executing bytecode from expression (fx+ 1 (fx+ 2 3))""" should "result in Fixnum(6)" in {

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
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 3, arg = 1),
      OpApplyPrimArg(unwind = false, next = false, nargs = 2, primNum = 226, arg = 1), // fx+
      OpImmediateLitToArg(value = 1, arg = 0),
      OpApplyPrimReg(unwind = false, next = false, nargs = 2, primNum = 226, reg = rslt), // fx+
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(RequestExpr)), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'argvec)(Tuple(Seq(Ob.NIV, Ob.NIV, Ob.NIV)))
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.rslt shouldBe Fixnum(6)
    end.ctxt.id shouldBe 1
  }

  "OpApplyCmd" should "not change ctxt after running primitive" in {

    /**
      * This opcode sequence was created manually and has no RBL code
      * counterpart.
      *
      * OpApplyCmd runs a primitive without saving its result.
      * Therefore the successful execution of a primitive through
      * OpApplyCmd should not change the ctxt object.
      */
    val start =
      testState
        .set(_ >> 'ctxt >> 'nargs)(2)
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(
      OpApplyCmd(unwind = false, next = false, nargs = 2, primNum = 226) // fx+
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt shouldBe start.ctxt
  }

  "OpApplyPrimTag" should "copy primitive result to location" in {

    /**
      * litvec:
      *   0:   {Loc Trgt}
      * codevec:
      *   0:   alloc 2
      *   1:   lit 1,arg[0]
      *   2:   lit 2,arg[1]
      *   3:   fx+ 2
      */
    val codevec = Seq(
      OpAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpApplyPrimTag(unwind = false, next = false, nargs = 2, primNum = 226, lit = 0) // fx+
    )

    val code = Code(Tuple(LocTrgt), codevec)

    val start =
      testState
        .set(_ >> 'code)(code)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.trgt shouldBe Fixnum(3)
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
    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtnReg(next = true, reg = trgt)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(Fixnum(100))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.trgt shouldBe Fixnum(100)
    end.ctxt.id shouldBe 1
  }

  "OpRtnArg" should "copy ctxt.rslt to given argvec element" in {

    /**
      * This opcode sequence will never be generated by the RBL
      * compiler as it is the responsibility of the caller to
      * specify where the result will be stored.
      *
      * litvec:
      *   0:   100
      * codevec:
      *   0:   liti 0,rslt
      *   1:   rtn/nxt arg[1]
      */
    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtnArg(next = true, arg = 1)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(Fixnum(100))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'argvec)(Tuple(Seq(Ob.NIV, Ob.NIV)))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.argvec.elem(1) shouldBe Fixnum(100)
    end.ctxt.id shouldBe 1
  }

  "OpRtnTag" should "copy ctxt.rslt to given location" in {

    /**
      * This opcode sequence will never be generated by the RBL
      * compiler as it is the responsibility of the caller to
      * specify where the result will be stored.
      *
      * litvec:
      *   0:   100
      *   1:   {Loc arg[1]}
      * codevec:
      *   0:   liti 0,rslt
      *   1:   rtn/tag/nxt
      */
    val codevec = Seq(
      OpIndLitToRslt(lit = 0),
      OpRtnTag(next = true, lit = 1)
    )

    val start =
      testState
        .set(_ >> 'code)(Code(Tuple(Seq(Fixnum(100), ArgRegister(1))), codevec))
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'argvec)(Tuple(Seq(Ob.NIV, Ob.NIV)))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.argvec.elem(1) shouldBe Fixnum(100)
    end.ctxt.id shouldBe 1
  }

  "OpXmitReg" should "save the result of xmit function into the specified register for future use" in {

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
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
      OpJmpFalse(15),
      OpAlloc(2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 1, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmit(unwind = false, next = true, 2),
      OpImmediateLitToReg(lit = 9, reg = rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'outstanding)(0)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(3)
  }

  "OpXmitRegXtnd" should "save the result of xmit function into the specified register for future use, the difference between this and OpXmitReg is that in rosette(C++) the maximum of nargs of this opcode is 256, instead of 16 in OpXmitReg" in {

    /**
      * This opcode sequence will be generated by the RBL
      * compiler with the expression(there should be more than 16 ones added):
      *
      * (and (+ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ) (+ 2 2))
      *
      * litvec:
      *   0:	{IfExpr}
      * codevec:
      *   0:	outstanding 23,1
      *   2:	push/alloc 16
      *   3:	lit 1,arg[0]
      *   4:	lit 1,arg[1]
      *   5:	lit 1,arg[2]
      *   6:	lit 1,arg[3]
      *   7:	lit 1,arg[4]
      *   8:	lit 1,arg[5]
      *   9:	lit 1,arg[6]
      *  10:	lit 1,arg[7]
      *  11:	lit 1,arg[8]
      *  12:	lit 1,arg[9]
      *  13:	lit 1,arg[10]
      *  14:	lit 1,arg[11]
      *  15:	lit 1,arg[12]
      *  16:	lit 1,arg[13]
      *  17:	lit 1,arg[14]
      *  18:	lit 1,arg[15]
      *  19:	xfer global[+],trgt
      *  21:	xmit/nxt 16,rslt
      *  23:	jf 30
      *  24:	alloc 2
      *  25:	lit 2,arg[0]
      *  26:	lit 2,arg[1]
      *  27:	xfer global[+],trgt
      *  29:	xmit/nxt 2
      *  30:	lit #f,rslt
      *  31:	rtn/nxt
      */
    val codevec = Seq(
      OpOutstanding(pc = 20, n = 1),
      OpPushAlloc(n = 16),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 1, arg = 1),
      OpImmediateLitToArg(value = 1, arg = 2),
      OpImmediateLitToArg(value = 1, arg = 3),
      OpImmediateLitToArg(value = 1, arg = 4),
      OpImmediateLitToArg(value = 1, arg = 5),
      OpImmediateLitToArg(value = 1, arg = 6),
      OpImmediateLitToArg(value = 1, arg = 7),
      OpImmediateLitToArg(value = 1, arg = 8),
      OpImmediateLitToArg(value = 1, arg = 9),
      OpImmediateLitToArg(value = 1, arg = 10),
      OpImmediateLitToArg(value = 1, arg = 11),
      OpImmediateLitToArg(value = 1, arg = 12),
      OpImmediateLitToArg(value = 1, arg = 13),
      OpImmediateLitToArg(value = 1, arg = 14),
      OpImmediateLitToArg(value = 1, arg = 15),
      OpXferGlobalToReg(reg = CtxtRegName.trgt, global = 668),
      OpXmitRegXtnd(unwind = false, next = true, nargs = 16, reg = CtxtRegName.rslt),
      OpJmpFalse(26),
      OpAlloc(2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = CtxtRegName.trgt, global = 668),
      OpXmit(unwind = false, next = true, 2),
      OpImmediateLitToReg(lit = 9, reg = CtxtRegName.rslt),
      OpRtn(next = true)
    )

    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'outstanding)(0)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'ctxt)(haltCtxt)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(4)
  }

  "OpXmitArg" should "save the result of xmit function into the specified argument for future use" in {

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
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpOutstanding(pc = 14, n = 2),
      OpPushAlloc(n = 2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 1, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmitArg(unwind = false, next = false, nargs = 2, arg = 1),
      OpPop(),
      OpPushAlloc(2),
      OpImmediateLitToArg(value = 2, arg = 0),
      OpImmediateLitToArg(value = 2, arg = 1),
      OpXferGlobalToReg(reg = trgt, global = 668),
      OpXmitArg(unwind = false, next = true, nargs = 2, arg = 0),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'outstanding)(0)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'ctxt)(Ctxt.empty)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'code)(Code(Tuple(Seq(LocRslt)),
                                                Seq(OpUpcallRtn(next = false, 0))))
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt)(Ctxt.empty)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'id)(2)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'code)(Code(Tuple.NIL, Seq(OpHalt())))

    val end = VirtualMachine.executeSeq(codevec, start)

    end.ctxt.rslt shouldBe Fixnum(7)
  }

  def executeSeqTest(opcodes: Seq[Op]): VMTransition[Unit] = State { vmState =>
    (VirtualMachine.executeSeq(opcodes, vmState), ())
  }

  "OpXmitArgXtnd" should "save the result of xmit function into the specified argument for future use, the argument is specified by the following byte" in {

    /**
      * One of the expressions that use the OpXmitArgXtnd shows below (there
      * should be more than 16 times '(+1 1)'), which compiled into about 124 opcodes.
      *
      * (+ (+ 1 1) (+ 1 1) (+ 1 1) (+ 1 1)
      *    (+ 1 1) (+ 1 1) (+ 1 1) (+ 1 1)
      *    (+ 1 1) (+ 1 1) (+ 1 1) (+ 1 1)
      *    (+ 1 1) (+ 1 1) (+ 1 1) (+ 1 1)
      *    (+ 1 1))
      *
      *
      * litvec:
      *   0:	{RequestExpr}
      * codevec:
      *   0:	alloc 17
      *   1:	xfer global[+],trgt
      *   3:	outstanding 124,17
      *   5:	push/alloc 2
      *   6:	lit 1,arg[0]
      *   7:	lit 1,arg[1]
      *   8:	xfer global[+],trgt
      *  10:	xmit 2,arg[16]
      *  12:	pop
      *  13:	push/alloc 2
      *  14:	lit 1,arg[0]
      *  15:	lit 1,arg[1]
      *  16:	xfer global[+],trgt
      *  18:	xmit 2,arg[15]
      *  19:	pop
      *  ...
      * 117:	pop
      * 118:	push/alloc 2
      * 119:	lit 1,arg[0]
      * 120:	lit 1,arg[1]
      * 121:	xfer global[+],trgt
      * 123:	xmit/nxt 2,arg[0]
      * 124:	xmit/nxt 17
      *
      */
    // this test does not reflect the described test case.
    val codevec = Seq(
      OpAlloc(n = 17),
      OpPushAlloc(2),
      OpImmediateLitToArg(value = 1, arg = 0),
      OpImmediateLitToArg(value = 1, arg = 1),
      OpXferGlobalToReg(reg = CtxtRegName.trgt, global = 668),
      OpXmitArgXtnd(unwind = false, next = false, nargs = 2, arg = 16),
    )

    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv))
        .set(_ >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'outstanding)(0)
        .set(_ >> 'ctxt >> 'id)(0)
        .set(_ >> 'ctxt >> 'code)(Code(Tuple.NIL, codevec))
        .set(_ >> 'ctxt >> 'ctxt)(Ctxt.empty)
        .set(_ >> 'ctxt >> 'ctxt >> 'id)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'outstanding)(1)
        .set(_ >> 'ctxt >> 'ctxt >> 'code)(Code(Tuple(Seq(LocRslt)),
                                                Seq(OpUpcallRtn(next = false, 0))))
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt)(Ctxt.empty)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'id)(2)
        .set(_ >> 'ctxt >> 'ctxt >> 'ctxt >> 'code)(Code(Tuple.NIL, Seq(OpHalt())))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.argvec.elem(16) shouldBe Fixnum(2)
  }
}
