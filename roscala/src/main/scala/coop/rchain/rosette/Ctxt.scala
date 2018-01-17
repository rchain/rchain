package coop.rchain.rosette

import coop.rchain.rosette.Location.StoreCtxt
import coop.rchain.rosette.VirtualMachine.loggerStrand
import Location._

case class Ctxt(tag: Location,
                nargs: Int,
                outstanding: Int,
                pc: PC,
                rslt: Ob, // reg[0]
                trgt: Ob, // reg[1]
                argvec: Tuple, // reg[2]
                env: Ob, // reg[3]
                code: Code, // reg[4]
                ctxt: Ctxt, // reg[5]
                self2: Ob, // reg[6]
                selfEnv: Ob, // reg[7]
                rcvr: Ob, // reg[8]
                monitor: Monitor) // reg[9]
    extends Ob {
  private val regs =
    Vector(rslt, trgt, argvec, env, code, ctxt, self2, selfEnv, rcvr, monitor)

  def applyK(result: Ob, tag: Location)(state: VMState): (Boolean, VMState) =
    this.ctxt.rcv(result, tag)(state)

  def arg(n: Int): Option[Ob] = argvec.elem.lift(n)

  def getReg(r: Int): Option[Ob] = regs.lift(r)

  /** This is necessary because the compiler sometimes arranges to
    *  provide an argvec that is acually longer than nargs indicates. If
    *  we are about to expose the context to the outside world, we need
    *  to clean it up so that it appears consistent.
    */
  def prepare(): Ctxt = this.copy(argvec = argvec.makeSlice(0, nargs))

  def rcv(result: Ob, loc: Location)(state: VMState): (Boolean, VMState) = {
    val storeResult = Location.store(loc, this, state.globalEnv, result)

    storeResult match {
      case StoreCtxt(newCtxt) =>
        val newState = state
          .set(_ >> 'ctxt >> 'ctxt)(newCtxt)
          .update(_ >> 'ctxt >> 'ctxt >> 'outstanding)(_ - 1)
          .updateSelf(st =>
            if (st.ctxt.ctxt.outstanding == 0) scheduleStrand(st) else st)

        (false, newState)

      // TODO: Add other store cases
      case _ =>
        (true, state)
    }
  }

  def ret(result: Ob)(state: VMState): (Boolean, VMState) =
    if (tag != LocLimbo) {
      applyK(result, this.tag)(state)
    } else {
      (false, state)
    }

  def scheduleStrand(state: VMState): VMState = {
    loggerStrand.info(s"Schedule strand ${state.ctxt.ctxt.hashCode()}")
    state.update(_ >> 'strandPool)(_ :+ state.ctxt.ctxt)
  }

  def setReg(r: Int, ob: Ob): Option[Ctxt] =
    r match {
      case 0 => Some(copy(rslt = ob))
      case 1 => Some(copy(trgt = ob))
      case 2 => Some(copy(argvec = ob.asInstanceOf[Tuple]))
      case 3 => Some(copy(env = ob))
      case 4 => Some(copy(code = ob.asInstanceOf[Code]))
      case 5 => Some(copy(ctxt = ob.asInstanceOf[Ctxt]))
      case 6 => Some(copy(self2 = ob))
      case 7 => Some(copy(selfEnv = ob))
      case 8 => Some(copy(rcvr = ob))
      case 9 => Some(copy(monitor = ob.asInstanceOf[Monitor]))
      case _ => None
    }

  def vmError(state: VMState): (Result, VMState) = {
    val newArgvec = Tuple(this.prepare())
    val newState = state.set(_ >> 'ctxt)(
      Ctxt(OprnVmError, newArgvec).copy(monitor = state.systemMonitor))

    OprnVmError.dispatch(newState)
  }
}

object Ctxt {
  def apply(tuple: Option[Tuple], ctxt: Ctxt): Ctxt = {
    val t = tuple.getOrElse(Tuple.Placeholder)
    Ctxt(
      tag = LocRslt,
      nargs = t.elem.size,
      outstanding = 0,
      pc = PC(0),
      rslt = Ob.NIV,
      trgt = Ob.NIV,
      argvec = t,
      env = ctxt.env,
      code = ctxt.code,
      ctxt = ctxt,
      self2 = ctxt.self2,
      selfEnv = ctxt.selfEnv,
      rcvr = ctxt.rcvr,
      monitor = ctxt.monitor
    )
  }

  def apply(trgt: Ob, argvec: Tuple): Ctxt = PLACEHOLDER

  object NIV
      extends Ctxt(null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)

  object PLACEHOLDER
      extends Ctxt(null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)
}
