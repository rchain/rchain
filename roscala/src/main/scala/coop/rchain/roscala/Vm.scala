package coop.rchain.roscala

import java.util.concurrent.RecursiveAction

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.Location._
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob._
import coop.rchain.roscala.pools.{StrandPool, StrandPoolExecutor}
import coop.rchain.roscala.prim.Prim
import coop.rchain.roscala.util.misc.OpcodePrettyPrinter

object Vm {

  /**
    * `State` holds the currently installed `Ctxt` and `Code`.
    * It also holds several control vars which are necessary
    * to control bytecode execution.
    * `code` usually equals to `ctxt.code` and `pc` is initially
    * set to `ctxt.pc`.
    */
  final case class State(
      var code: Code = null,
      var ctxt: Ctxt = null,
      var doNextThreadFlag: Boolean = false,
      var exitFlag: Boolean = false,
      var nextOpFlag: Boolean = false,
      var pc: Int = 0,
      var vmErrorFlag: Boolean = false,
      globalEnv: GlobalEnv
  )(val strandPool: StrandPool)

  def run[E: StrandPoolExecutor](ctxt: Ctxt, state: StrandPool => State): Unit = {
    val strandPool = StrandPoolExecutor.instance[E]
    val vm         = new Vm(ctxt, state(strandPool))
    StrandPoolExecutor.start(vm)
    vm.logger.debug("Exiting the VM")
  }
}

class Vm(val ctxt0: Ctxt, val state0: State) extends RecursiveAction {
  val logger              = Logger("Vm")
  val opcodePrettyPrinter = new OpcodePrettyPrinter()

  override def compute(): Unit = run(ctxt0, state0)

  /**
    * Install a `Ctxt` and runs `Ctxt.code`
    *
    * Mutates `ctxt`, `globalEnv` and `state`.
    * Runs until `doExitFlag` is set or there are no more opcodes.
    * Also tries to fetch new work from `state.strandPool`
    */
  private def run(ctxt: Ctxt, state: State): Unit = {
    // Install `ctxt`
    state.ctxt = ctxt
    state.code = ctxt.code
    state.pc = ctxt.pc

    while (state.pc < state.code.codevec.size && !state.exitFlag) {
      val opcode = state.code.codevec(state.pc)

      // Indented debug output
      logger.debug(s"${state.ctxt} - " + opcodePrettyPrinter.print(state.pc, opcode))

      state.pc += 1

      // Execute `opcode`
      execute(opcode, state.globalEnv, state)
      executeFlags(state)
    }

    logger.debug("Exiting run method")

    state.strandPool.finish()
  }

  /**
    * Mutates `ctxt`, `globalEnv` and `state`
    */
  def execute(opcode: Opcode, globalEnv: GlobalEnv, state: State): Unit =
    opcode match {
      /* Halts the VM */
      case OpHalt =>
        state.exitFlag = true

      /**
        * Creates a new `Ctxt` from the current one.
        * The current `Ctxt` becomes the continuation of the newly
        * created `Ctxt`. The new `Ctxt` then becomes the installed
        * `Ctxt` in the VM.
        */
      case OpPush =>
        state.ctxt = Ctxt(state.ctxt)
        state.nextOpFlag = true

      /**
        * Installs the continuation of the current `Ctxt`.
        * Throws away the installed `Ctxt`.
        */
      case OpPop =>
        state.ctxt = state.ctxt.ctxt.get
        state.nextOpFlag = true

      /**
        * Sets `nargs` fields of current `Ctxt` to `n`, which says how
        * many arguments there are in the `argvec` of the installed
        * `Ctxt`.
        */
      case OpNargs(n) =>
        state.ctxt.nargs = n
        state.nextOpFlag = true

      /**
        * Allocates a new `Tuple` of size `n` which becomes the new
        * `argvec` of the installed `Ctxt`.
        */
      case OpAlloc(n) =>
        state.ctxt.argvec = Tuple(new Array[Ob](n))
        state.nextOpFlag = true

      /**
        * Same as `OpPush` with the difference that the newly created
        * `Ctxt` contains an `argvec` of size `n`.
        */
      case OpPushAlloc(n) =>
        val t = Tuple(new Array[Ob](n))
        state.ctxt = Ctxt(t, state.ctxt)
        state.nextOpFlag = true

      /**
        * Gets a `Template` from position `lit` in the `litvec` of the
        * installed `Ctxt`. A `Template` contains the formal parameters.
        * Then extends the `env` of the current `Ctxt` with the formals
        * bound to the actuals.
        */
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

              // TODO: Fix output - output is not ordered correctly
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

      /**
        * Sets `pc` field of installed `Ctxt` to `pc` and
        * `outstanding` field to `n`. This is usually done before a
        * child `Ctxt` is spawned (e.g. with `OpPushAlloc`).
        * The `outstanding` field tells the spawned child `Ctxt`s how
        * many arguments are missing in their continuation.
        * This is important because if a child `Ctxt` sees that it is
        * the last child `Ctxt` to provide a result back to its
        * continuation, it is the job of the child to schedule the
        * continuation. The `pc` field says where to continue in the
        * bytecode once all child `Ctxt` returned their results back.
        */
      case OpOutstanding(pc, n) =>
        state.ctxt.pc = pc
        state.ctxt.outstanding.set(n)
        state.nextOpFlag = true

      /**
        * Copies the installed `Ctxt` and sets the `pc` field of the
        * copy to `pc`. Then schedules the copy which once executed
        * will start at the position `pc`.
        */
      case OpFork(pc) =>
        val newCtxt = state.ctxt.clone()
        newCtxt.pc = pc
        logger.debug(s"Fork: Schedule cloned ctxt ($newCtxt)")
        state.strandPool.prepend((newCtxt, state))
        state.nextOpFlag = true

      /**
        * Does the same thing as `OpXmit` with the difference of
        * providing a location to the `tag` register of the installed
        * `Ctxt`. The location will tell the dispatch method of the
        * object in the `trgt` register where to write results to (in
        * the parent `Ctxt`).
        * `OpXmitTag` will take a location from the `litvec`.
        */
      case OpXmitTag(unwind, next, nargs, lit) =>
        state.ctxt.nargs = nargs
        state.ctxt.tag = state.code.litvec(lit).asInstanceOf[Location]
        doXmit(next, unwind, state, globalEnv)

      /**
        * Creates a `Location` for a particular location in the `argvec`.
        * Then behaves like `OpXmitTag`.
        */
      case OpXmitArg(unwind, next, nargs, arg) =>
        state.ctxt.nargs = nargs
        state.ctxt.tag = ArgRegister(arg)
        doXmit(next, unwind, state, globalEnv)

      /**
        * Creates a `Location` for a particular register in `state.ctxt`.
        * Then behaves like `OpXmitTag`.
        */
      case OpXmitReg(unwind, next, nargs, reg) =>
        state.ctxt.nargs = nargs
        state.ctxt.tag = CtxtRegister(reg)
        doXmit(next, unwind, state, globalEnv)

      /**
        * Executes the `dispatch` method on whatever is currently
        * in the `trgt` register of the installed `Ctxt`. This can
        * lead to all kind of things including the scheduling of `Ctxt`s.
        * Usually the `dispatch` method will return back a result by
        * writing it into the position described by a location in the
        * `tag` field of the parent `Ctxt`. The `dispatch` method
        * returns a result which usually equals to `Suspended`.
        * In this case the VM goes to the next opcode.
        * If `next` is set, the installed `Ctxt` gives up control
        * to the VM. The VM will then get the next available `Ctxt`.
        */
      case OpXmit(unwind, next, nargs) =>
        state.ctxt.nargs = nargs
        doXmit(next, unwind, state, globalEnv)

      /**
        * Behaves like `OpXmit` with the difference that results will
        * be ignored and will not get written anywhere.
        */
      case OpSend(unwind, next, nargs) =>
        state.ctxt.ctxt = None
        state.ctxt.nargs = nargs
        state.ctxt.tag = LocLimbo
        doXmit(next, unwind, state, globalEnv)

      /**
        * Executes a primitive and puts the result into a location.
        * `OpApplyPrimTag` gets the location from the `litvec`.
        * `opApplyPrimArg` saves to a particular position in the
        * `argvec`. `OpApplyPrimReg` saves to a particular register.
        * `OpApplyCmd` discards results.
        * If the `next` flag is set, the installed `Ctxt` gives up
        * control to the VM.
        * The VM will then get the next available `Ctxt`.
        */
      case OpApplyPrimTag(unwind, next, nargs, primNum, lit) =>
        state.ctxt.nargs = nargs
        val prim = Prim.nthPrim(primNum)
        val result =
          if (unwind) unwindAndApplyPrim(prim, state, globalEnv)
          else prim.dispatchHelper(state)
        val location = state.ctxt.code.litvec(lit).asInstanceOf[Location]

        if (result == Deadthread)
          state.doNextThreadFlag = true
        else {
          if (store(location, state.ctxt, result))
            state.vmErrorFlag = true
          else {
            logger.debug(s"OpApplyPrimTag: Store $result from $prim to $location")
            if (next)
              state.doNextThreadFlag = true
            else
              state.nextOpFlag = true
          }
        }

      /* See `OpApplyPrimTag` */
      case OpApplyPrimArg(unwind, next, nargs, primNum, arg) =>
        state.ctxt.nargs = nargs
        val prim = Prim.nthPrim(primNum)
        val result =
          if (unwind) unwindAndApplyPrim(prim, state, globalEnv)
          else prim.dispatchHelper(state)

        if (result == Deadthread)
          state.doNextThreadFlag = true
        else if (arg >= state.ctxt.argvec.numberOfElements())
          state.vmErrorFlag = true
        else {
          logger.debug(s"OpApplyPrimArg: Store $result from $prim to arg[$arg]")
          state.ctxt.argvec.update(arg, result)
          if (next)
            state.doNextThreadFlag = true
          else
            state.nextOpFlag = true
        }

      /* See `OpApplyPrimTag` */
      case OpApplyPrimReg(unwind, next, nargs, primNum, reg) =>
        state.ctxt.nargs = nargs
        val prim = Prim.nthPrim(primNum)
        val result =
          if (unwind) unwindAndApplyPrim(prim, state, globalEnv)
          else prim.dispatchHelper(state)

        if (result == Deadthread)
          state.doNextThreadFlag = true
        else {
          logger.debug(s"OpApplyPrimReg: Store $result from $prim to ${regName(reg)}")
          state.ctxt.setReg(reg, result)
          if (next)
            state.doNextThreadFlag = true
          else
            state.nextOpFlag = true
        }

      /* See `OpApplyPrimTag` */
      case OpApplyCmd(unwind, next, nargs, primNum) =>
        state.ctxt.nargs = nargs
        val prim = Prim.nthPrim(primNum)
        val result =
          if (unwind) unwindAndApplyPrim(prim, state, globalEnv)
          else prim.dispatchHelper(state)

        if (result == Deadthread)
          state.doNextThreadFlag = true
        else {
          if (next)
            state.doNextThreadFlag = true
          else
            state.nextOpFlag = true
        }

      /**
        * Returns the content of the `rslt` register of the installed
        * `Ctxt` to its continuation. The `tag` field of the installed
        * `Ctxt` determines the location in the continuation where the
        * VM will write the result (content of the `rslt` register) to.
        * The different versions of `OpRtn` only differ in the
        * locations they put into the `tag` field before they return
        * the result. If the `next` flag is set, the installed `Ctxt`
        * gives up control to the VM. The VM will then get the next
        * available `Ctxt`. Returning the result to the continuation
        * can lead to the scheduling of the continuation. This is the
        * case if the return was the last return the continuation was
        * waiting for and is described by the `outstanding` field in
        * the continuation.
        */
      case OpRtn(next) =>
        doRtn(next, state)

      /* See `OpRtn` */
      case OpRtnTag(next, lit) =>
        state.ctxt.tag = state.code.litvec(lit).asInstanceOf[Location]
        doRtn(next, state)

      /* See `OpRtn` */
      case OpRtnArg(next, arg) =>
        state.ctxt.tag = ArgRegister(arg)
        doRtn(next, state)

      /* See `OpRtn` */
      case OpRtnReg(next, reg) =>
        state.ctxt.tag = CtxtRegister(reg)
        doRtn(next, state)

      /**
        * Does the same as `OpRtn` with the difference that
        * it will always schedule the continuation and by
        * default sets the `tag` register to `litvec[lit]`.
        */
      case OpUpcallRtn(next, lit) =>
        state.ctxt.tag = state.code.litvec(lit).asInstanceOf[Location]
        if (store(state.ctxt.tag, state.ctxt.ctxt.get, state.ctxt.rslt)) {
          state.vmErrorFlag = true
        } else {
          if (next) {
            state.doNextThreadFlag = true
          } else {
            state.ctxt.ctxt.get.scheduleStrand(state)
            state.doNextThreadFlag = true
          }
        }

      /**
        * Schedules the current continuation and then forces the
        * scheduler to install the next available `Ctxt`.
        */
      case OpUpcallResume =>
        state.ctxt.ctxt.get.scheduleStrand(state)
        state.doNextThreadFlag = true

      /**
        * Forces the scheduler to install the next available `Ctxt`.
        */
      case OpNxt =>
        state.doNextThreadFlag = true

      /**
        * Sets `state.ctxt.env` of the installed `Ctxt` to an ancestor
        * of `state.ctxt.env` defined by `cut`. Then sets the program
        * counter of the VM to `pc`.
        */
      case OpJmpCut(pc, cut) =>
        var newEnv = state.ctxt.env
        for (_ <- 0 until cut) newEnv = newEnv.parent
        state.ctxt.env = newEnv
        state.pc = pc
        state.nextOpFlag = true

      /**
        * Sets the program counter of the VM to `pc`.
        */
      case OpJmp(pc) =>
        state.pc = pc
        state.nextOpFlag = true

      /**
        * Sets the program counter to `pc` if the `rslt` register of
        * the installed `Ctxt` equals `RblFalse`.
        */
      case OpJmpFalse(pc) =>
        if (state.ctxt.rslt == RblBool(false)) {
          logger.debug(s"Jump to $pc")
          state.pc = pc
        }
        state.nextOpFlag = true

      /**
        * Looks up a value for a given key (defined by `litvec[lit]`)
        * in `state.ctxt.selfEnv`. Then puts the value into
        * `argvec[arg]`.
        */
      case OpLookupToArg(lit, arg) =>
        val key   = state.code.litvec(lit)
        val value = state.ctxt.selfEnv.meta.lookupObo(state.ctxt.selfEnv, key, globalEnv)

        if (value == Upcall) {
          state.ctxt.pc = state.pc
          state.doNextThreadFlag = true
        } else if (value == Absent) {
          // TODO: Add `handleMissingBinding`
          state.doNextThreadFlag = true
        } else {
          state.ctxt.argvec.update(arg, value)
          state.nextOpFlag = true
        }

      /**
        * Looks up a value for a given key (defined by `litvec[lit]`)
        * in `state.ctxt.selfEnv`. Then puts the value into the
        * register defined by `reg`.
        */
      case OpLookupToReg(lit, reg) =>
        val key = state.code.litvec(lit)
        logger.debug(s"Lookup $key in ${state.ctxt.selfEnv}")
        val value = state.ctxt.selfEnv.meta.lookupObo(state.ctxt.selfEnv, key, globalEnv)

        if (value == Upcall) {
          state.ctxt.pc = state.pc
          state.doNextThreadFlag = true
        } else if (value == Absent) {
          // TODO: Add `handleMissingBinding`
          state.doNextThreadFlag = true
        } else {
          state.ctxt.setReg(reg, value)
          state.nextOpFlag = true
        }

      /**
        * Copy the slot entry, defined by `indirect`, `level` and
        * `offset`, of the current environment (`state.ctxt.env`)
        * to `argvec[arg]`.
        */
      case OpXferLexToArg(indirect, level, offset, arg) =>
        var env = state.ctxt.env
        for (_ <- 0 until level) env = env.parent

        if (indirect) env = env.asInstanceOf[Actor].extension

        state.ctxt.argvec.update(arg, env.slot.unsafeGet(offset))
        logger.debug(
          s"Xfer ${env.slot.unsafeGet(offset)} from lex[$level, $offset] to argvec[$arg]"
        )
        state.nextOpFlag = true

      /**
        * Copy the slot entry, defined by `indirect`, `level` and
        * `offset` of the current environment (`state.ctxt.env`), to
        * the register defined by `reg`.
        */
      case OpXferLexToReg(indirect, level, offset, reg) =>
        var env = state.ctxt.env
        for (_ <- 0 until level) env = env.parent

        if (indirect) env = env.asInstanceOf[Actor].extension

        state.ctxt.setReg(reg, env.slot.unsafeGet(offset))
        logger.debug(
          s"Xfer ${env.slot.unsafeGet(offset)} from lex[$level, $offset] to ${regName(reg)}"
        )
        state.nextOpFlag = true

      /**
        * Copy value at position `global` in the global environment
        * to `argvec[arg]`.
        */
      case OpXferGlobalToArg(global, arg) =>
        val ob = globalEnv.values(global)
        state.ctxt.argvec.update(arg, ob)
        state.nextOpFlag = true

      /**
        * Copy value at position `global` in the global environment
        * to the register defined by `reg`.
        */
      case OpXferGlobalToReg(global, reg) =>
        val ob = globalEnv.values(global)
        state.ctxt.setReg(reg, ob)
        state.nextOpFlag = true

      /**
        * Copy `argvec[src]` to `argvec[dest]`.
        */
      case OpXferArgToArg(dest, src) =>
        val ob = state.ctxt.arg(src)
        state.ctxt.argvec.update(dest, ob)
        state.nextOpFlag = true

      /**
        * Copy `rslt` register to `argvec[arg]`.
        */
      case OpXferRsltToArg(arg) =>
        val rslt = state.ctxt.rslt
        state.ctxt.argvec.update(arg, rslt)
        state.nextOpFlag = true

      /**
        * Copy `argvec[arg]` to `rslt` register.
        */
      case OpXferArgToRslt(arg) =>
        val ob = state.ctxt.arg(arg)
        state.ctxt.rslt = ob
        state.nextOpFlag = true

      /**
        * Copy `rslt` to register defined by `reg`.
        */
      case OpXferRsltToReg(reg) =>
        val rslt = state.ctxt.rslt
        state.ctxt.setReg(reg, rslt)
        state.nextOpFlag = true

      /**
        * Copy content of register defined by `reg` to `rslt`
        * register.
        */
      case OpXferRegToRslt(reg) =>
        state.ctxt.rslt = state.ctxt.reg(reg)
        state.nextOpFlag = true

      /**
        * Gets a location from position `litvec[lit]`.
        * Then stores the current content of the `rslt` register into
        * the given location.
        */
      case OpXferRsltToDest(lit) =>
        val location = state.code.litvec(lit).asInstanceOf[Location]
        logger.debug(s"Xfer ${state.ctxt.rslt} to $location")
        if (store(location, state.ctxt, state.ctxt.rslt))
          state.vmErrorFlag = true
        else
          state.nextOpFlag = true

      /**
        * Gets a location from position `litvec[lit]`.
        * Then fetches an object from that location and puts it into
        * the `rslt` register.
        */
      case OpXferSrcToRslt(lit) =>
        val location = state.code.litvec(lit).asInstanceOf[Location]
        state.ctxt.rslt = fetch(location, state.ctxt, globalEnv)
        state.nextOpFlag = true

      /**
        * Copy `litvec[lit]` to `argvec[arg]`.
        */
      case OpIndLitToArg(lit, arg) =>
        val ob = state.code.litvec(lit)
        state.ctxt.argvec.update(arg, ob)
        state.nextOpFlag = true

      /**
        * Copy `litvec[lit]` to the register defined by `reg`.
        */
      case OpIndLitToReg(lit, reg) =>
        val ob = state.code.litvec(lit)
        state.ctxt.setReg(reg, ob)
        state.nextOpFlag = true

      /**
        * Copy `litvec[lit]` to the `rslt` register.
        */
      case OpIndLitToRslt(lit) =>
        val ob = state.code.litvec(lit)
        state.ctxt.rslt = ob
        state.nextOpFlag = true

      /**
        * Copy the VM literal defined by `literal` to `argvec[arg]`.
        */
      case OpImmediateLitToArg(literal, arg) =>
        state.ctxt.argvec.update(arg, vmLiterals(literal))
        state.nextOpFlag = true

      /**
        * Copy the VM literal defined by `literal` to the register
        * defined by `reg`.
        */
      case OpImmediateLitToReg(literal, reg) =>
        state.ctxt.setReg(reg, vmLiterals(literal))
        state.nextOpFlag = true
    }

  def executeFlags(state: Vm.State): Unit =
    if (state.doNextThreadFlag) {
      if (state.strandPool.getNextStrand(state))
        state.exitFlag = true
      else {
        state.nextOpFlag = true
        state.doNextThreadFlag = false
      }
    }

  /**
    * Return `rslt`
    *
    * Returns content of `rslt` register of to its continuation.
    * If the continuation has no outstanding arguments, the continuation
    * gets scheduled.
    */
  def doRtn(next: Boolean, state: State): Unit = {
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
  def doXmit(next: Boolean, unwind: Boolean, state: State, globalEnv: GlobalEnv): Unit = {
    val result =
      if (unwind) unwindAndDispatch(state, globalEnv)
      else state.ctxt.trgt.dispatch(state.ctxt, state)

    if (result == Deadthread)
      state.doNextThreadFlag = true
    else if (next)
      state.doNextThreadFlag = true
    else
      state.nextOpFlag = true
  }

  def unwindAndApplyPrim(prim: Prim, state: State, globalEnv: GlobalEnv): Ob = {
    val suffix           = state.ctxt.argvec.value.last
    val oldArgvec        = state.ctxt.argvec
    val oldNargs         = state.ctxt.nargs
    var newArgvec: Tuple = Nil

    if (state.ctxt.nargs != 0 || suffix != Nil) {
      if (!suffix.isInstanceOf[Tuple])
        // TODO: Add runtime error
        return Deadthread

      newArgvec = state.ctxt.argvec.unwind()
    }

    state.ctxt.argvec = newArgvec
    state.ctxt.nargs = newArgvec.numberOfElements()

    val result = prim.dispatchHelper(state)
    state.ctxt.argvec = oldArgvec
    state.ctxt.nargs = oldNargs
    result
  }

  def unwindAndDispatch(state: State, globalEnv: GlobalEnv): Ob = {
    val suffix           = state.ctxt.argvec.value.last
    var newArgvec: Tuple = Nil

    if (state.ctxt.nargs != 0 || suffix != Nil) {
      if (!suffix.isInstanceOf[Tuple])
        // TODO: Add runtime error
        return Deadthread

      newArgvec = state.ctxt.argvec.unwind()
    }

    state.ctxt.argvec = newArgvec
    state.ctxt.nargs = 0

    state.ctxt.trgt.dispatch(state.ctxt, state)
  }

}
