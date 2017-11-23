package coop.rchain.rosette

import coop.rchain.rosette.Meta.StdMeta
import org.scalatest._
import coop.rchain.rosette.utils.opcodes._

class TransitionSpec extends FlatSpec with Matchers {
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
    val stdOprnPlus = StdOprn(null, null)
    val stdMeta = StdMeta()
    val globalEnv = Seq.fill(669)(Ob.NIV).updated(668, stdOprnPlus)
    val start =
      testState
        .set(_ >> 'globalEnv)(TblObject(globalEnv, null))
        .set(_ >> 'ctxt >> 'tag)(LocationGT(Location.LTCtxtRegister(0)))
        .set(_ >> 'ctxt >> 'ctxt)(testState.ctxt)

    val codevec = Seq(OpAlloc(2),
                      OpImmediateLitToArg(1, 0),
                      OpImmediateLitToArg(2, 1),
                      OpXferGlobalToReg(1, 668),
                      OpXmit(u = false, n = true, 2))

    val end = VirtualMachine.executeSeq(codevec, start)
    end.ctxt.ctxt.rslt shouldBe Fixnum(3)
  }

}
