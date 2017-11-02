package coop.rchain.rosette

sealed trait Op
case class OpHalt() extends Op
case class OpPush() extends Op
case class OpPop() extends Op
case class OpNargs(n: Int) extends Op
case class OpAlloc(n: Int) extends Op
case class OpPushAlloc(n: Int) extends Op
case class OpExtend(v: Int) extends Op
case class OpOutstanding(p: Int, n: Int) extends Op
case class OpFork(p: Int) extends Op
case class OpXmitTag(u: Boolean, n: Boolean, m: Int, v: Int) extends Op
case class OpXmitArg(u: Boolean, n: Boolean, m: Int, a: Int) extends Op
case class OpXmitReg(u: Boolean, n: Boolean, m: Int, r: Int) extends Op
case class OpXmit(u: Boolean, n: Boolean, m: Int) extends Op
case class OpXmitTagXtnd(u: Boolean, n: Boolean, m: Int, v: Int) extends Op
case class OpXmitArgXtnd(u: Boolean, n: Boolean, m: Int, a: Int) extends Op
case class OpXmitRegXtnd(u: Boolean, n: Boolean, m: Int, r: Int) extends Op
case class OpSend(u: Boolean, n: Boolean, m: Int) extends Op
case class OpApplyPrimTag(u: Boolean, n: Boolean, m: Int, k: Int, v: Int)
    extends Op
case class OpApplyPrimArg(u: Boolean, n: Boolean, m: Int, k: Int, a: Int)
    extends Op
case class OpApplyPrimReg(u: Boolean, n: Boolean, m: Int, k: Int, r: Int)
    extends Op
case class OpApplyCmd(u: Boolean, n: Boolean, m: Int, k: Int) extends Op
case class OpRtnTag(n: Boolean, v: Int) extends Op
case class OpRtnArg(n: Boolean, a: Int) extends Op
case class OpRtnReg(n: Boolean, r: Int) extends Op
case class OpRtn(n: Boolean) extends Op
case class OpUpcallRtn(n: Boolean, v: Int) extends Op
case class OpUpcallResume() extends Op
case class OpNxt() extends Op
case class OpJmp(n: Int) extends Op
case class OpJmpFalse(n: Int) extends Op
case class OpJmpCut(n: Int, m: Int) extends Op
case class OpLookupToArg(a: Int, v: Int) extends Op
case class OpLookupToReg(r: Int, v: Int) extends Op
case class OpXferLexToArg(i: Boolean, l: Int, o: Int, a: Int) extends Op
case class OpXferLexToReg(i: Boolean, l: Int, o: Int, r: Int) extends Op
case class OpXferGlobalToArg(a: Int, g: Int) extends Op
case class OpXferGlobalToReg(r: Int, g: Int) extends Op
case class OpXferArgToArg(d: Int, s: Int) extends Op
case class OpXferRsltToArg(a: Int) extends Op
case class OpXferArgToRslt(a: Int) extends Op
case class OpXferRsltToReg(r: Int) extends Op
case class OpXferRegToRslt(r: Int) extends Op
case class OpXferRsltToDest(v: Int) extends Op
case class OpXferSrcToRslt(v: Int) extends Op
case class OpIndLitToArg(a: Int, v: Int) extends Op
case class OpIndLitToReg(r: Int, v: Int) extends Op
case class OpIndLitToRslt(v: Int) extends Op
case class OpImmediateLitToArg(v: Int, a: Int) extends Op
case class OpImmediateLitToReg(v: Int, r: Int) extends Op
case class OpUnknown() extends Op
