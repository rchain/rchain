package coop.rchain.roscala

sealed trait Opcode
case class OpAlloc(n: Int)                                                      extends Opcode
case class OpExtend(lit: Int)                                                   extends Opcode
case class OpFork(pc: Int)                                                      extends Opcode
case class OpImmediateLitToArg(literal: Int, arg: Int)                          extends Opcode
case class OpImmediateLitToReg(literal: Int, reg: Int)                          extends Opcode
case class OpIndLitToArg(lit: Int, arg: Int)                                    extends Opcode
case class OpJmp(pc: Int)                                                       extends Opcode
case class OpJmpFalse(pc: Int)                                                  extends Opcode
case class OpNargs(n: Int)                                                      extends Opcode
case class OpOutstanding(pc: Int, n: Int)                                       extends Opcode
case class OpPushAlloc(n: Int)                                                  extends Opcode
case class OpRtn(next: Boolean)                                                 extends Opcode
case class OpXferGlobalToArg(global: Int, arg: Int)                             extends Opcode
case class OpXferGlobalToReg(global: Int, reg: Int)                             extends Opcode
case class OpXferLexToReg(indirect: Boolean, level: Int, offset: Int, reg: Int) extends Opcode
case class OpXferRsltToDest(lit: Int)                                           extends Opcode
case class OpXmit(unwind: Boolean, next: Boolean, nargs: Int)                   extends Opcode
case class OpXmitArg(unwind: Boolean, next: Boolean, nargs: Int, arg: Int)      extends Opcode
