package coop.rchain.rosette

sealed trait Op
case class OpHalt() extends Op
case class OpPush() extends Op
case class OpPop() extends Op
case class OpNargs(n: Int) extends Op
case class OpAlloc(n: Int) extends Op
case class OpPushAlloc(n: Int) extends Op
case class OpExtend(v: Int) extends Op
case class OpOutstanding(pc: Int, n: Int) extends Op
case class OpFork(pc: Int) extends Op
case class OpXmitTag(unwind: Boolean, next: Boolean, nargs: Int, v: Int)
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
case class OpRtnTag(n: Boolean, v: Int) extends Op
case class OpRtnArg(n: Boolean, arg: Int) extends Op
case class OpRtnReg(n: Boolean, reg: Int) extends Op
case class OpRtn(n: Boolean) extends Op
case class OpUpcallRtn(n: Boolean, v: Int) extends Op
case class OpUpcallResume() extends Op
case class OpNxt() extends Op
case class OpJmp(pc: Int) extends Op
case class OpJmpFalse(pc: Int) extends Op
case class OpJmpCut(pc: Int, m: Int) extends Op
case class OpLookupToArg(arg: Int, v: Int) extends Op
case class OpLookupToReg(reg: Int, v: Int) extends Op
case class OpXferLexToArg(i: Boolean, l: Int, o: Int, arg: Int) extends Op
case class OpXferLexToReg(i: Boolean, l: Int, o: Int, reg: Int) extends Op
case class OpXferGlobalToArg(arg: Int, g: Int) extends Op
case class OpXferGlobalToReg(reg: Int, g: Int) extends Op
case class OpXferArgToArg(dest: Int, src: Int) extends Op
case class OpXferRsltToArg(arg: Int) extends Op
case class OpXferArgToRslt(arg: Int) extends Op
case class OpXferRsltToReg(reg: Int) extends Op
case class OpXferRegToRslt(reg: Int) extends Op
case class OpXferRsltToDest(v: Int) extends Op
case class OpXferSrcToRslt(v: Int) extends Op
case class OpIndLitToArg(arg: Int, lit: Int) extends Op
case class OpIndLitToReg(r: Int, lit: Int) extends Op
case class OpIndLitToRslt(v: Int) extends Op
case class OpImmediateLitToArg(value: Int, arg: Int) extends Op
case class OpImmediateLitToReg(lit: Int, reg: Int) extends Op
case class OpUnknown() extends Op
