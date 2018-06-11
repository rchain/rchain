package coop.rchain.roscala

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Location._
import coop.rchain.roscala.ob._

import scala.collection.mutable

object Vm {
  val logger = Logger("Vm")

  /**
    * `State` holds the currently installed `Ctxt` and `Code`.
    * It also holds several control vars which are necessary
    * to control bytecode execution.
    * `code` usually equals to `ctxt.code` and `pc` is initially
    * set to `ctxt.pc`.
    */
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
      state.pc += 1

      // execute `opcode`
      execute(opcode, globalEnv, state)
      executeFlags(globalEnv, state)
    }
  }

  /**
    * Mutates `ctxt`, `globalEnv` and `state`
    */
  def execute(opcode: Opcode, globalEnv: GlobalEnv, state: State): Unit =
    opcode match {
      case OpAlloc(n) =>
        state.ctxt.argvec = Tuple(new Array[Ob](n))
        state.nextOpFlag = true

      case OpNargs(n) =>
        state.ctxt.nargs = n
        state.nextOpFlag = true

      case OpPushAlloc(n) =>
        val t = Tuple(new Array[Ob](n))
        state.ctxt = Ctxt(t, state.ctxt)
        state.nextOpFlag = true

      case OpExtend(lit) =>
        val formals    = state.code.litvec(lit).asInstanceOf[Template]
        val optActuals = formals.matchPattern(state.ctxt.argvec, state.ctxt.nargs)

        optActuals match {
          case Some(actuals) =>
            /**
              * Creates an `Extension` that maps from formals (e.g. `Symbol(x)`)
              * to actuals (e.g. `Fixnum(1)`).
              * Formals are given in `formals.keyMeta` and `actuals` is a `Tuple`.
              * The current `env` becomes the parent of the newly created `Extension`.
              */
            val map = formals.keyMeta.map

            map.useWithReadLock { m =>
              import collection.JavaConverters._

              val keys = m.keys.asScala.toIterable

              val pairsStr = keys
                .zip(actuals.value)
                .map { case (key, value) => s"$key -> $value" }
                .mkString(", ")
              logger.debug(s"Extend current env with: $pairsStr")

              val extended = state.ctxt.env.extendWith(formals.keyMeta, actuals)
              state.ctxt.env = extended
              state.ctxt.nargs = 0
              state.nextOpFlag = true
            }

          case None =>
            state.doNextThreadFlag = true
        }

      case OpOutstanding(pc, n) =>
        state.ctxt.pc = pc
        state.ctxt.outstanding = n
        state.nextOpFlag = true

      case OpIndLitToArg(arg, lit) =>
        val ob = state.code.litvec(lit)
        state.ctxt.argvec.update(arg, ob)
        state.nextOpFlag = true

      case OpImmediateLitToReg(literal, reg) =>
        state.ctxt.setReg(reg, vmLiterals(literal))
        state.nextOpFlag = true

      case OpImmediateLitToArg(literal, arg) =>
        state.ctxt.argvec.update(arg, vmLiterals(literal))
        state.nextOpFlag = true

      case OpJmp(pc) =>
        state.pc = pc
        state.nextOpFlag = true

      case OpJmpFalse(pc) =>
        if (state.ctxt.rslt == RblFalse) {
          logger.debug(s"Jump to $pc")
          state.pc = pc
        }
        state.nextOpFlag = true

      case OpRtn(next) =>
        doRtn(next, state)
        if (next)
          state.doNextThreadFlag = true
        else
          state.nextOpFlag = true

      case OpXferGlobalToReg(global, reg) =>
        val ob = globalEnv.values(global)
        state.ctxt.setReg(reg, ob)
        state.nextOpFlag = true

      case OpXferGlobalToArg(global, arg) =>
        val ob = globalEnv.values(global)
        state.ctxt.argvec.update(arg, ob)
        state.nextOpFlag = true

      case OpXferLexToReg(indirect, level, offset, reg) =>
        var env = state.ctxt.env
        for (_ <- 0 until level) env = env.parent

        if (indirect) env = env.asInstanceOf[Actor].extension

        logger.debug(s"Xfer ${env.slot(offset)} from lex[$level, $offset] to ${regName(reg)}")

        state.ctxt.setReg(reg, env.slot.unsafeGet(offset))
        state.nextOpFlag = true

      case OpXferRsltToDest(lit) =>
        val loc = state.code.litvec(lit).asInstanceOf[Location]
        logger.debug(s"Xfer ${state.ctxt.rslt} to $loc")

        if (store(loc, state.ctxt, state.ctxt.rslt))
          state.vmErrorFlag = true
        else
          state.nextOpFlag = true

      case OpFork(pc) =>
        val newCtxt = state.ctxt.clone()
        newCtxt.pc = pc
        state.strandPool.prepend(newCtxt)
        state.nextOpFlag = true

      case OpXmit(_, next, nargs) =>
        logger.debug(s"doXmit${if (next) "/nxt"} $nargs")
        state.ctxt.nargs = nargs
        doXmit(next, state, globalEnv)

      case OpXmitArg(_, next, nargs, arg) =>
        logger.debug(s"doXmit${if (next) "/nxt"} $nargs,arg[$arg]")
        state.ctxt.nargs = nargs
        state.ctxt.tag = ArgRegister(arg)
        doXmit(next, state, globalEnv)
    }

  def executeFlags(env: GlobalEnv, state: Vm.State): Unit =
    if (state.doNextThreadFlag) {
      if (getNextStrand(state))
        state.exitFlag = true
      else
        state.nextOpFlag = true
    }

  def getNextStrand(state: State): Boolean =
    if (state.strandPool.isEmpty) {
      logger.debug("Empty strandPool - exiting VM")
      true
    } else {
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
  def doXmit(next: Boolean, state: State, globalEnv: GlobalEnv): Unit = {
    val result = state.ctxt.trgt.dispatch(state, globalEnv)

    if (result == Deadthread)
      state.doNextThreadFlag = true
    else if (next)
      state.doNextThreadFlag = true
    else
      state.nextOpFlag = true
  }

}
