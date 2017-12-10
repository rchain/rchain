package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta
import coop.rchain.rosette.expr.{LetExpr, TupleExpr}
import org.scalatest._

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

    val codevec = Seq(OpImmediateLitToReg(v = 8, r = 0),
                      OpJmpFalse(4),
                      OpImmediateLitToReg(v = 1, r = 0),
                      OpRtn(n = true),
                      OpImmediateLitToReg(v = 2, r = 0),
                      OpRtn(n = true))

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

    val codevec = Seq(OpImmediateLitToReg(v = 9, r = 0),
                      OpJmpFalse(4),
                      OpImmediateLitToReg(v = 1, r = 0),
                      OpRtn(n = true),
                      OpImmediateLitToReg(v = 2, r = 0),
                      OpRtn(n = true))

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
                      OpImmediateLitToArg(v = 1, a = 0),
                      OpImmediateLitToArg(v = 2, a = 1),
                      OpXferGlobalToReg(r = 1, g = 668),
                      OpXmit(u = false, n = true, 2))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(3)
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
      OpImmediateLitToArg(v = 1, a = 0),
      OpImmediateLitToArg(v = 2, a = 1),
      OpNargs(2),
      OpExtend(1),
      OpAlloc(2),
      OpXferLexToArg(i = false, l = 0, o = 0, a = 0),
      OpXferLexToArg(i = false, l = 0, o = 1, a = 1),
      OpXferGlobalToReg(r = 1, g = 668),
      OpXmit(u = false, n = true, 2)
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
    // b src/Vm.cc if code->codevec->instr(0)->word == 3079
    val start =
      testState
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)
        .set(_ >> 'globalEnv)(TblObject(globalEnv))

    val codevec = Seq(
      OpFork(6),
      OpAlloc(2),
      OpImmediateLitToArg(v = 1, a = 0),
      OpImmediateLitToArg(v = 2, a = 1),
      OpXferGlobalToReg(r = 1, g = 668),
      OpXmit(u = false, n = true, 2),
      OpAlloc(2),
      OpImmediateLitToArg(v = 3, a = 0),
      OpImmediateLitToArg(v = 4, a = 1),
      OpXferGlobalToReg(r = 1, g = 668),
      OpXmit(u = false, n = true, 2)
    )

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt should (be(Fixnum(3)) or be(Fixnum(7)))
  }
}
