package coop.rchain.rosette

sealed trait Op
case class OpHalt() extends Op
case class OpPush() extends Op
case class OpPop() extends Op
case class OpNargs(next: Int) extends Op
case class OpAlloc(next: Int) extends Op
case class OpPushAlloc(next: Int) extends Op
case class OpExtend(v: Int) extends Op
case class OpOutstanding(pc: Int, n: Int) extends Op
case class OpFork(pc: Int) extends Op
case class OpXmitTag(u: Boolean, n: Boolean, m: Int, v: Int) extends Op
case class OpXmitArg(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)
    extends Op
case class OpXmitReg(unwind: Boolean, next: Boolean, nargs: Int, reg: Int)
    extends Op
case class OpXmit(unwind: Boolean, next: Boolean, nargs: Int) extends Op
case class OpXmitTagXtnd(unwind: Boolean, next: Boolean, nargs: Int, v: Int)
    extends Op
case class OpXmitArgXtnd(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)
    extends Op
case class OpXmitRegXtnd(unwind: Boolean, next: Boolean, nargs: Int, reg: Int)
    extends Op
case class OpSend(u: Boolean, n: Boolean, m: Int) extends Op
case class OpApplyPrimTag(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          k: Int,
                          v: Int)
    extends Op
case class OpApplyPrimArg(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          k: Int,
                          a: Int)
    extends Op
case class OpApplyPrimReg(unwind: Boolean,
                          next: Boolean,
                          nargs: Int,
                          k: Int,
                          r: Int)
    extends Op
case class OpApplyCmd(unwind: Boolean, next: Boolean, nargs: Int, k: Int)
    extends Op
case class OpRtnTag(next: Boolean, v: Int) extends Op
case class OpRtnArg(next: Boolean, arg: Int) extends Op
case class OpRtnReg(next: Boolean, reg: Int) extends Op
case class OpRtn(next: Boolean) extends Op
case class OpUpcallRtn(next: Boolean, v: Int) extends Op
case class OpUpcallResume() extends Op
case class OpNxt() extends Op
case class OpJmp(next: Int) extends Op
case class OpJmpFalse(next: Int) extends Op
case class OpJmpCut(next: Int, m: Int) extends Op
case class OpLookupToArg(arg: Int, v: Int) extends Op
case class OpLookupToReg(reg: Int, v: Int) extends Op
case class OpXferLexToArg(i: Boolean, l: Int, o: Int, a: Int) extends Op
case class OpXferLexToReg(i: Boolean, l: Int, o: Int, r: Int) extends Op
case class OpXferGlobalToArg(arg: Int, g: Int) extends Op
case class OpXferGlobalToReg(reg: Int, g: Int) extends Op
case class OpXferArgToArg(dest: Int, src: Int) extends Op
case class OpXferRsltToArg(arg: Int) extends Op
case class OpXferArgToRslt(arg: Int) extends Op
case class OpXferRsltToReg(reg: Int) extends Op
case class OpXferRegToRslt(reg: Int) extends Op
case class OpXferRsltToDest(v: Int) extends Op
case class OpXferSrcToRslt(v: Int) extends Op
case class OpIndLitToArg(arg: Int, v: Int) extends Op
case class OpIndLitToReg(r: Int, v: Int) extends Op
case class OpIndLitToRslt(v: Int) extends Op
case class OpImmediateLitToArg(value: Int, arg: Int) extends Op
case class OpImmediateLitToReg(v: Int, reg: Int) extends Op
case class OpUnknown() extends Op
