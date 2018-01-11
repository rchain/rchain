package coop.rchain.rosette

sealed trait Op
case class OpHalt() extends Op
case class OpPush() extends Op
case class OpPop() extends Op
case class OpNargs(nargs: Int) extends Op
case class OpAlloc(n: Int) extends Op
case class OpPushAlloc(n: Int) extends Op
case class OpExtend(lit: Int) extends Op
case class OpOutstanding(pc: Int, n: Int) extends Op
case class OpFork(pc: Int) extends Op
case class OpXmitTag(unwind: Boolean, next: Boolean, nargs: Int, lit: Int)
    extends Op
case class OpXmitArg(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)
    extends Op
case class OpXmitReg(unwind: Boolean, next: Boolean, nargs: Int, reg: Int)
    extends Op
case class OpXmit(unwind: Boolean, next: Boolean, nargs: Int) extends Op
case class OpXmitTagXtnd(unwind: Boolean, next: Boolean, nargs: Int, lit: Int)
    extends Op
case class OpXmitArgXtnd(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)
    extends Op
case class OpXmitRegXtnd(unwind: Boolean, next: Boolean, nargs: Int, reg: Int)
    extends Op
case class OpSend(unwind: Boolean, next: Boolean, nargs: Int) extends Op
case class OpApplyPrimTag(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          primNum: Int,
                          lit: Int)
    extends Op
case class OpApplyPrimArg(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          primNum: Int,
                          arg: Int)
    extends Op
case class OpApplyPrimReg(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          primNum: Int,
                          reg: Int)
    extends Op
case class OpApplyCmd(unwind: Boolean, next: Boolean, nargs: Int, primNum: Int)
    extends Op
case class OpRtnTag(next: Boolean, lit: Int) extends Op
case class OpRtnArg(next: Boolean, arg: Int) extends Op
case class OpRtnReg(next: Boolean, reg: Int) extends Op
case class OpRtn(next: Boolean) extends Op
case class OpUpcallRtn(next: Boolean, lit: Int) extends Op
case class OpUpcallResume() extends Op
case class OpNxt() extends Op
case class OpJmp(pc: Int) extends Op
case class OpJmpFalse(pc: Int) extends Op
case class OpJmpCut(pc: Int, cut: Int) extends Op
case class OpLookupToArg(arg: Int, lit: Int) extends Op
case class OpLookupToReg(reg: Int, lit: Int) extends Op
case class OpXferLexToArg(indirect: Boolean, level: Int, offset: Int, arg: Int)
    extends Op
case class OpXferLexToReg(indirect: Boolean, level: Int, offset: Int, reg: Int)
    extends Op
case class OpXferGlobalToArg(arg: Int, global: Int) extends Op
case class OpXferGlobalToReg(reg: Int, global: Int) extends Op
case class OpXferArgToArg(dest: Int, src: Int) extends Op
case class OpXferRsltToArg(arg: Int) extends Op
case class OpXferArgToRslt(arg: Int) extends Op
case class OpXferRsltToReg(reg: Int) extends Op
case class OpXferRegToRslt(reg: Int) extends Op
case class OpXferRsltToDest(lit: Int) extends Op
case class OpXferSrcToRslt(lit: Int) extends Op
case class OpIndLitToArg(arg: Int, lit: Int) extends Op
case class OpIndLitToReg(reg: Int, lit: Int) extends Op
case class OpIndLitToRslt(lit: Int) extends Op
case class OpImmediateLitToArg(value: Int, arg: Int) extends Op
case class OpImmediateLitToReg(lit: Int, reg: Int) extends Op
case class OpUnknown() extends Op
