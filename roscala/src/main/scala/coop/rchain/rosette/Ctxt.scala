package coop.rchain.rosette

import scala.collection.mutable

case class Ctxt(argvec: Tuple,
                code: Code,
                ctxt: Ctxt,
                env: Ob,
                monitor: Monitor,
                nargs: Int,
                outstanding: Int,
                pc: PC,
                reg: Seq[Ob],
                rslt: Ob,
                trgt: Ob,
                selfEnv: Ob,
                override val _slot: mutable.Seq[Ob],
                tag: Location)
    extends Ob {
  def arg(n: Int): Option[Ob] = argvec.elem.lift(n)

  /** This is necessary because the compiler sometimes arranges to
    *  provide an argvec that is acually longer than nargs indicates. If
    *  we are about to expose the context to the outside world, we need
    *  to clean it up so that it appears consistent.
    */
  def prepare(): Ctxt = this.copy(argvec = argvec.makeSlice(0, nargs))

  def ret(rslt: Ob): Boolean = true

  def scheduleStrand(state: VMState): VMState =
    state.update(_ >> 'strandPool)(_ :+ this)

  def vmError(state: VMState): Ob = {
    val newArgvec = Tuple(this.prepare())
    val newCtxt =
      Ctxt(OprnVmError, newArgvec).copy(monitor = state.systemMonitor)

    OprnVmError.dispatch(newCtxt)
  }
}

object Ctxt {
  def apply(tuple: Option[Tuple], ctxt: Ctxt): Ctxt = PLACEHOLDER

  def apply(trgt: Ob, argvec: Tuple): Ctxt = PLACEHOLDER

  object NIV
      extends Ctxt(null,
                   null,
                   null,
                   null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)

  object PLACEHOLDER
      extends Ctxt(null,
                   null,
                   null,
                   null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)
}
