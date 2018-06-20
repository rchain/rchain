package coop.rchain.roscala

sealed trait Opcode
case class OpAlloc(n: Int)                                                      extends Opcode
case class OpApplyCmd(unwind: Boolean, next: Boolean, nargs: Int, primNum: Int) extends Opcode
case class OpApplyPrimArg(unwind: Boolean, next: Boolean, nargs: Int, primNum: Int, arg: Int)
    extends Opcode
case class OpApplyPrimReg(unwind: Boolean, next: Boolean, nargs: Int, primNum: Int, reg: Int)
    extends Opcode
case class OpApplyPrimTag(unwind: Boolean, next: Boolean, nargs: Int, primNum: Int, lit: Int)
    extends Opcode
case class OpExtend(lit: Int)                                                   extends Opcode
case class OpFork(pc: Int)                                                      extends Opcode
case class OpImmediateLitToArg(literal: Int, arg: Int)                          extends Opcode
case class OpImmediateLitToReg(literal: Int, reg: Int)                          extends Opcode
case class OpIndLitToArg(lit: Int, arg: Int)                                    extends Opcode
case class OpIndLitToReg(lit: Int, reg: Int)                                    extends Opcode
case class OpIndLitToRslt(lit: Int)                                             extends Opcode
case class OpJmp(pc: Int)                                                       extends Opcode
case class OpJmpCut(pc: Int, cut: Int)                                          extends Opcode
case class OpJmpFalse(pc: Int)                                                  extends Opcode
case class OpLookupToArg(lit: Int, arg: Int)                                    extends Opcode
case class OpLookupToReg(lit: Int, reg: Int)                                    extends Opcode
case class OpNargs(n: Int)                                                      extends Opcode
case class OpOutstanding(pc: Int, n: Int)                                       extends Opcode
case class OpPushAlloc(n: Int)                                                  extends Opcode
case class OpRtn(next: Boolean)                                                 extends Opcode
case class OpRtnArg(next: Boolean, arg: Int)                                    extends Opcode
case class OpRtnReg(next: Boolean, reg: Int)                                    extends Opcode
case class OpRtnTag(next: Boolean, lit: Int)                                    extends Opcode
case class OpSend(unwind: Boolean, next: Boolean, nargs: Int)                   extends Opcode
case class OpUpcallRtn(next: Boolean, lit: Int)                                 extends Opcode
case class OpXferArgToArg(dest: Int, src: Int)                                  extends Opcode
case class OpXferArgToRslt(arg: Int)                                            extends Opcode
case class OpXferGlobalToArg(global: Int, arg: Int)                             extends Opcode
case class OpXferGlobalToReg(global: Int, reg: Int)                             extends Opcode
case class OpXferLexToArg(indirect: Boolean, level: Int, offset: Int, arg: Int) extends Opcode
case class OpXferLexToReg(indirect: Boolean, level: Int, offset: Int, reg: Int) extends Opcode
case class OpXferRegToRslt(reg: Int)                                            extends Opcode
case class OpXferRsltToArg(arg: Int)                                            extends Opcode
case class OpXferRsltToDest(lit: Int)                                           extends Opcode
case class OpXferRsltToReg(reg: Int)                                            extends Opcode
case class OpXferSrcToRslt(lit: Int)                                            extends Opcode
case class OpXmit(unwind: Boolean, next: Boolean, nargs: Int)                   extends Opcode
case class OpXmitArg(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)      extends Opcode
case class OpXmitReg(unwind: Boolean, next: Boolean, nargs: Int, reg: Int)      extends Opcode
case class OpXmitTag(unwind: Boolean, next: Boolean, nargs: Int, lit: Int)      extends Opcode
case object OpHalt                                                              extends Opcode
case object OpNxt                                                               extends Opcode
case object OpPop                                                               extends Opcode
case object OpPush                                                              extends Opcode
case object OpUpcallResume                                                      extends Opcode
