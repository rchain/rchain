package coop.rchain.rosette

import shapeless._
import shapeless.OpticDefns.RootLens

case class VMState(bytecodes: Map[Op, Long],
                   code: Code,
                   ctxt: Ctxt,
                   loc: Location,
                   pc: PC,
                   strandPool: Seq[Ctxt] = Seq(),
                   sleeperPool: Seq[Ctxt] = Seq(),
                   sigvec: Int = 0,
                   nsigs: Int = 0,
                   systemMonitor: Monitor = null,
                   currentMonitor: Monitor = null,
                   debug: Boolean = false,
                   debugInfo: Seq[String] = Seq(),
                   nextOpFlag: Boolean = true,
                   doXmitFlag: Boolean = false,
                   xmitData: (Boolean, Boolean) = (false, false),
                   doRtnFlag: Boolean = false,
                   doRtnData: Boolean = false,
                   doNextThreadFlag: Boolean = false,
                   doAsyncWaitFlag: Boolean = false,
                   vmErrorFlag: Boolean = false,
                   exitFlag: Boolean = false,
                   exitCode: Int = 0,
                   obCounts: Long = 0,
                   interruptPending: Int = 0,
                   globalEnv: TblObject = TblObject.PLACEHOLDER)
    extends {
  def set[T](f: RootLens[VMState] ⇒ Lens[VMState, T])(value: T): VMState =
    f(lens[VMState]).set(this)(value)

  def update[T](f: RootLens[VMState] ⇒ Lens[VMState, T])(
      value: T => T): VMState =
    f(lens[VMState]).modify(this)(value)

  def updateSelf[T](value: VMState => VMState): VMState = value(this)
}
