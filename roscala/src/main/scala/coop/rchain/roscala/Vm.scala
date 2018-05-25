package coop.rchain.roscala

import coop.rchain.roscala.ob._
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

object Vm {
  val logger = Logger("Vm")

  final case class State(strandPool: mutable.Buffer[Ctxt] = mutable.Buffer(),
                         var code: Code = null,
                         var ctxt: Ctxt = null,
                         var doNextThreadFlag: Boolean = false,
                         var exitFlag: Boolean = false,
                         var nextOpFlag: Boolean = false,
                         var pc: Int = 0,
                         var vmErrorFlag: Boolean = false)

  /**
    * Install a `Ctxt` and runs `Ctxt.code`
    *
    * Mutates `ctxt`, `globalEnv` and `state`.
    * Runs until `doExitFlag` is set or there are no more opcodes.
    * Also tries to fetch new work from `state.strandPool`
    */
  def run(ctxt: Ctxt, globalEnv: GlobalEnv, state: State): Unit = {
    // Install `ctxt`
    state.ctxt = ctxt
    state.code = ctxt.code
    state.pc = ctxt.pc

    while (state.pc < state.code.codevec.size && !state.exitFlag) {
      val opcode = state.code.codevec(state.pc)

      // execute `opcode`
      execute(opcode, globalEnv, state)
      executeFlags(globalEnv, state)

      state.pc += 1
    }
  }

  /**
    * Mutates `ctxt`, `globalEnv` and `state`
    */
  def execute(opcode: Opcode, globalEnv: GlobalEnv, state: State): Unit =
    opcode match {
      case OpAlloc(n) =>
        state.ctxt.argvec = Tuple(new Array[Ob](n))

      case OpIndLitToArg(arg, lit) =>
        val ob = state.code.litvec(lit)
        state.ctxt.argvec.update(arg, ob)

      case OpImmediateLitToReg(literal, reg) =>
        state.ctxt.setReg(reg, vmLiterals(literal))

      case OpJmpFalse(pc) =>
        if (state.ctxt.rslt == RblFalse) {
          logger.debug(s"Jump to $pc")
          state.pc = pc
        }

      case OpRtn(next) =>
        doRtn(next, state)
        if (next)
          state.doNextThreadFlag = true
        else
          state.nextOpFlag = true

      case OpXferGlobalToReg(global, reg) =>
        val ob = globalEnv.values(global)
        state.ctxt.setReg(reg, ob)

      case OpXferGlobalToArg(global, arg) =>
        val ob = globalEnv.values(global)
        state.ctxt.argvec.update(arg, ob)

      case OpXmit(unwind, next, nargs) =>
        state.ctxt.nargs = nargs
        doXmit(next, state)
    }

  def executeFlags(env: GlobalEnv, state: Vm.State): Unit =
    if (state.doNextThreadFlag) {
      if (getNextStrand(state))
        state.exitFlag = true
      else
        state.nextOpFlag = true
    }

  def getNextStrand(state: State): Boolean =
    if (state.strandPool.isEmpty)
      true
    else {
      logger.debug("Install ctxt")

      val ctxt = state.strandPool.remove(state.strandPool.size - 1)

      // Install `ctxt`
      state.ctxt = ctxt
      state.code = ctxt.code
      state.pc = ctxt.pc

      false
    }

  /**
    * Return `rslt`
    *
    * Returns content of `rslt` register of to its continuation.
    * If the continuation has no outstanding arguments, the continuation
    * gets scheduled.
    */
  def doRtn(next: Boolean, state: State): Unit = {
    logger.debug(s"doRtn${if (next) "/nxt"}")

    val result = state.ctxt.rslt

    if (state.ctxt.ret(result, state))
      state.vmErrorFlag = true
    else if (next)
      state.doNextThreadFlag = true
    else
      state.nextOpFlag = true
  }

  /**
    * Dispatch messages to `trgt`
    *
    * Runs `dispatch` method of the object in the `trgt` register.
    * The current `Ctxt`, `state.ctxt`, contains the messages that
    * are dispatched.
    *
    * Results of the `dispatch` method will be written into `state.ctxt`.
    * `dispatch` can also schedule new `Ctxt`s.
    */
  def doXmit(next: Boolean, state: State): Unit = {
    logger.debug(s"doXmit${if (next) "/nxt"}")
    val result = state.ctxt.trgt.dispatch(state.ctxt)

    if (result == Deadthread)
      state.doNextThreadFlag = true
    else if (next)
      state.doNextThreadFlag = true
    else
      state.nextOpFlag = true
  }

}
