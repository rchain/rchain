package coop.rchain.rosette

import com.typesafe.scalalogging.Logger
import coop.rchain.rosette.Ob._
import coop.rchain.rosette.prim.Prim

sealed trait Work
case object NoWorkLeft extends Work
case object WaitForAsync extends Work
case class StrandsScheduled(state: VMState) extends Work

object VirtualMachine {
  val loggerOpcode = Logger("opcode")
  val loggerStrand = Logger("strand")

  val unknownRegister: Int => String = reg => s"Unknown register: $reg"

  val vmLiterals: Seq[Ob] = Seq(
    Fixnum(0),
    Fixnum(1),
    Fixnum(2),
    Fixnum(3),
    Fixnum(4),
    Fixnum(5),
    Fixnum(6),
    Fixnum(7),
    Ob.RBLTRUE,
    Ob.RBLFALSE,
    Tuple.NIL,
    Ob.NIV
  )

  def handleApplyPrimSuspend(op: Op): Unit = ()
  def handleApplyPrimUpcall(op: Op, tag: Location): Unit = ()
  def handleFormalsMismatch(formals: Template): Ob = null
  def handleMissingBinding(key: Ob, argReg: Location): Ob = null
  def handleSleep(): Unit = ()
  def handleXmitUpcall(op: Op, tag: Location): Unit = ()

  def handleVirtualMachineError(state: VMState): VMState =
    state.ctxt.vmError(state)._2

  /**
    *  This code protects the current argvec, temporarily replacing it
    *  with the unwound argvec for use by the primitive, and then
    *  restoring it after the primitive has finished.  This is necessary
    *  because of the way that the compiler permits inlined primitives
    *  (the subjects of opApplyPrim opcodes) to share a common argvec.
    *  Unwinding cannot be permitted to clobber the argvec that the
    *  compiler has set up, or bad things can happen (and they are *hard*
    *  to track down).
    */
  def unwindAndApplyPrim(prim: Prim, state: VMState): (Result, VMState) =
    state.ctxt.argvec.flattenRest() match {
      case Right(newArgvec) =>
        val tmpState = state
          .set(_ >> 'ctxt >> 'argvec)(newArgvec)
          .set(_ >> 'ctxt >> 'nargs)(newArgvec.elem.size)

        val result = prim.dispatchHelper(tmpState.ctxt)

        (result, state)

      case Left(AbsentRest) =>
        val tmpState = state
          .set(_ >> 'ctxt >> 'argvec)(Tuple.NIL)
          .set(_ >> 'ctxt >> 'nargs)(0)

        val result = prim.dispatchHelper(tmpState.ctxt)

        (result, state)

      case Left(InvalidRest) =>
        val (error, errorState) =
          prim.runtimeError("&rest value is not a tuple", state)

        (Left(error), errorState)
    }

  def handleException(v: Ob, op: Op, tag: Location): Unit =
    v.sysval match {
      case SyscodeUpcall =>
        op match {
          case _: OpApplyCmd | _: OpApplyPrimArg | _: OpApplyPrimReg |
              _: OpApplyPrimTag =>
            handleApplyPrimUpcall(op, tag)
          case _ =>
            handleXmitUpcall(op, tag)
        }

      case SyscodeSuspend =>
        op match {
          case _: OpApplyCmd | _: OpApplyPrimArg | _: OpApplyPrimReg |
              _: OpApplyPrimTag =>
            handleApplyPrimSuspend(op)

          case _ => // Nothing happens; this is the usual case.
        }

      case SyscodeInterrupt => suicide("what to do with syscodeInterrupt?")

      case SyscodeSleep => handleSleep()

      case SyscodeInvalid | SyscodeDeadThread => // We don't do diddly

      case _ => suicide(s"unknown SysCode value (${v.sysval})")
    }

  def getNextStrand(state: VMState): (Boolean, VMState) = {
    loggerStrand.info("Try to get next strand")

    if (state.strandPool.isEmpty) {
      tryAwakeSleepingStrand(state) match {
        case WaitForAsync =>
          val newState = state.set(_ >> 'doAsyncWaitFlag)(true)
          (false, newState)

        case NoWorkLeft => (true, state)

        case StrandsScheduled(stateScheduled) =>
          val stateDebug = if (stateScheduled.debug) {
            state.update(_ >> 'debugInfo)(_ :+ "*** waking sleepers\n")
          } else {
            stateScheduled
          }

          val strand = stateDebug.strandPool.head
          val newState = stateDebug.update(_ >> 'strandPool)(_.tail)

          (false, installStrand(strand, newState))
      }
    } else {
      val strand = state.strandPool.head
      val newState = state.update(_ >> 'strandPool)(_.tail)

      (false, installStrand(strand, newState))
    }
  }

  def tryAwakeSleepingStrand(state: VMState): Work =
    if (state.sleeperPool.isEmpty) {
      if (state.nsigs == 0) {
        NoWorkLeft
      } else {
        WaitForAsync
      }
    } else {

      /** Schedule all sleeping strands
        *
        * Pop strand from sleeperPool and enqueue
        * to strandPool
        */
      val scheduled = state.sleeperPool
        .foldLeft(state) {
          case (st, sleeper) =>
            sleeper.scheduleStrand(st)
        }
        .set(_ >> 'sleeperPool)(Seq())

      StrandsScheduled(scheduled)
    }

  def installStrand(strand: Ctxt, state: VMState): VMState = {
    val stateInstallMonitor =
      if (strand.monitor != state.currentMonitor)
        installMonitor(strand.monitor, state)
      else state

    loggerStrand.info(s"Install strand ${strand.hashCode()}")
    installCtxt(strand, stateInstallMonitor)
  }

  def installMonitor(monitor: Monitor, state: VMState): VMState = {
    val stateDebug = if (state.debug) {
      state.update(_ >> 'debugInfo)(_ :+ s"*** new monitor: ${monitor.id}\n")
    } else {
      state
    }

    stateDebug.currentMonitor.stop()

    val newState = stateDebug
      .set(_ >> 'bytecodes)(monitor.opcodeCounts)
      .set(_ >> 'currentMonitor)(monitor)
      .set(_ >> 'debug)(monitor.tracing)
      .set(_ >> 'obCounts)(monitor.obCounts)

    newState.currentMonitor
      .start()

    newState
  }

  def installCtxt(ctxt: Ctxt, state: VMState): VMState = {
    val stateDebug = if (state.debug) {
      state.update(_ >> 'debugInfo)(_ :+ "*** new strand\n")
    } else {
      state
    }

    stateDebug
      .set(_ >> 'ctxt)(ctxt)
      .set(_ >> 'code)(ctxt.code)
      .set(_ >> 'pc >> 'relative)(ctxt.pc.relative)
  }

  def executeSeq(opCodes: Seq[Op], state: VMState): VMState = {
    var pc = state.pc.relative
    var exit = false
    var currentState = state

    while (pc < opCodes.size && !exit) {
      val op = opCodes(pc)
      loggerOpcode.info("PC: " + pc + " Opcode: " + op)

      currentState = currentState
        .update(_ >> 'pc >> 'relative)(_ + 1)
        .update(_ >> 'bytecodes)(
          _.updated(op, currentState.bytecodes.getOrElse(op, 0.toLong) + 1))

      currentState = runFlags(executeDispatch(op, currentState))

      pc = currentState.pc.relative

      if (currentState.exitFlag) exit = true
    }

    loggerOpcode.info("Exiting")
    currentState
  }

  def runFlags(state: VMState): VMState = {
    var mState = state

    if (mState.doXmitFlag) {
      // may set doNextThreadFlag
      mState = doXmit(mState).set(_ >> 'doXmitFlag)(false)
    }

    if (mState.doRtnFlag) {
      // may set doNextThreadFlag
      mState = doRtn(mState).set(_ >> 'doRtnFlag)(false)
    }

    if (mState.vmErrorFlag) {
      // TODO: Revisit once OprnVmError works
      //handleVirtualMachineError(mState)
      mState = mState.set(_ >> 'doNextThreadFlag)(true)
    }

    if (mState.doNextThreadFlag) {
      val (isEmpty, tmpState) = getNextStrand(mState)
      mState = tmpState.set(_ >> 'doNextThreadFlag)(false)

      if (isEmpty) {
        mState = mState.set(_ >> 'exitFlag)(true)
      }
    }

    mState
  }

  /** Stops the VM and appends a message to state.debugInfo if debugging is enabled */
  def die(msg: String)(state: VMState): VMState =
    state
      .set(_ >> 'exitFlag)(true)
      .set(_ >> 'exitCode)(1)
      .update(_ >> 'debugInfo)(info => if (state.debug) info :+ msg else info)

  def setCtxtReg(reg: Int, ob: Ob)(state: VMState): VMState =
    state.ctxt.setReg(reg, ob) match {
      case Some(newCtxt) => state.set(_ >> 'ctxt)(newCtxt)
      case None => die(unknownRegister(reg))(state)
    }

  def getCtxtReg(reg: Int)(state: VMState): (Option[Ob], VMState) =
    state.ctxt.getReg(reg) match {
      case someOb @ Some(_) => (someOb, state)
      case None => (None, die(unknownRegister(reg))(state))
    }

  def doRtn(state: VMState): VMState = {
    val (isError, newState) = state.ctxt.ret(state.ctxt.rslt)(state)

    if (isError)
      newState.set(_ >> 'vmErrorFlag)(true)
    else if (newState.doRtnFlag)
      newState.set(_ >> 'doNextThreadFlag)(true)
    else
      newState
  }

  def doXmit(state: VMState): VMState = {
    val (result, newState) = state.ctxt.trgt match {
      case ob: StdOprn => ob.dispatch(state)
      // TODO: Add other cases
      case _ => (Right(Ob.NIV), state)
    }

    result match {
      case Right(ob) if ob.is(OTsysval) =>
        // handleException(ob, instr, state.ctxt.tag)
        newState.set(_ >> 'doNextThreadFlag)(true)
      case Left(DeadThread) => newState.set(_ >> 'doNextThreadFlag)(true)
      case _ if state.xmitData._2 => newState.set(_ >> 'doNextThreadFlag)(true)
      case _ => newState
    }
  }

  def executeDispatch(op: Op, state: VMState): VMState =
    op match {
      case o: OpHalt => execute(o, state)
      case o: OpPush => execute(o, state)
      case o: OpPop => execute(o, state)
      case o: OpNargs => execute(o, state)
      case o: OpPushAlloc => execute(o, state)
      case o: OpExtend => execute(o, state)
      case o: OpOutstanding => execute(o, state)
      case o: OpAlloc => execute(o, state)
      case o: OpFork => execute(o, state)
      case o: OpXmitTag => execute(o, state)
      case o: OpXmitArg => execute(o, state)
      case o: OpXmitReg => execute(o, state)
      case o: OpXmit => execute(o, state)
      case o: OpXmitTagXtnd => execute(o, state)
      case o: OpXmitArgXtnd => execute(o, state)
      case o: OpXmitRegXtnd => execute(o, state)
      case o: OpSend => execute(o, state)
      case o: OpApplyPrimTag => execute(o, state)
      case o: OpApplyPrimArg => execute(o, state)
      case o: OpApplyPrimReg => execute(o, state)
      case o: OpApplyCmd => execute(o, state)
      case o: OpRtnTag => execute(o, state)
      case o: OpRtnArg => execute(o, state)
      case o: OpRtnReg => execute(o, state)
      case o: OpRtn => execute(o, state)
      case o: OpUpcallRtn => execute(o, state)
      case o: OpUpcallResume => execute(o, state)
      case o: OpNxt => execute(o, state)
      case o: OpJmp => execute(o, state)
      case o: OpJmpFalse => execute(o, state)
      case o: OpJmpCut => execute(o, state)
      case o: OpLookupToArg => execute(o, state)
      case o: OpLookupToReg => execute(o, state)
      case o: OpXferLexToArg => execute(o, state)
      case o: OpXferLexToReg => execute(o, state)
      case o: OpXferGlobalToArg => execute(o, state)
      case o: OpXferGlobalToReg => execute(o, state)
      case o: OpXferArgToArg => execute(o, state)
      case o: OpXferRsltToArg => execute(o, state)
      case o: OpXferArgToRslt => execute(o, state)
      case o: OpXferRsltToReg => execute(o, state)
      case o: OpXferRegToRslt => execute(o, state)
      case o: OpXferRsltToDest => execute(o, state)
      case o: OpXferSrcToRslt => execute(o, state)
      case o: OpIndLitToArg => execute(o, state)
      case o: OpIndLitToReg => execute(o, state)
      case o: OpIndLitToRslt => execute(o, state)
      case o: OpImmediateLitToArg => execute(o, state)
      case o: OpImmediateLitToReg => execute(o, state)
      case o: OpUnknown => execute(o, state)
    }

  def execute(op: OpHalt, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(0)

  def execute(op: OpPush, state: VMState): VMState =
    state.set(_ >> 'ctxt)(Ctxt(None, state.ctxt))

  def execute(op: OpPop, state: VMState): VMState =
    state.set(_ >> 'ctxt)(state.ctxt.ctxt)

  def execute(op: OpNargs, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'nargs)(op.nargs)

  def execute(op: OpAlloc, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'argvec)(Tuple(op.n, NIV))

  def execute(op: OpPushAlloc, state: VMState): VMState =
    state.update(_ >> 'ctxt)(Ctxt(Some(Tuple(op.n, None)), _))

  def execute(op: OpExtend, state: VMState): VMState = {
    def getTemplate = state.code.lit(op.lit).as[Template]
    def matchActuals(template: Template) =
      template.`match`(state.ctxt.argvec, state.ctxt.nargs)
    def getStdExtension = state.ctxt.env.as[StdExtension]

    val newState = for {
      template <- getTemplate or die(
        s"OpExtend: No template in state.code.litvec(${op.lit})")(state)

      tuple <- matchActuals(template) or
        // TODO: Revisit
        //handleFormalsMismatch(formals)
        state.set(_ >> 'doNextThreadFlag)(true)

      env <- getStdExtension or die(
        "OpExtend: state.ctxt.env needs to be a StdExtension")(state)

    } yield {
      val newEnv = env.extendWith(template.keyMeta, tuple)
      state
        .set(_ >> 'ctxt >> 'env)(newEnv)
        .set(_ >> 'ctxt >> 'nargs)(0)
    }

    newState.merge
  }

  def execute(op: OpOutstanding, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'pc)(PC(op.pc))
      .set(_ >> 'ctxt >> 'outstanding)(op.n)

  def execute(op: OpFork, state: VMState): VMState = {
    val newCtxt = state.ctxt.copy(pc = PC(op.pc))
    state.update(_ >> 'strandPool)(newCtxt +: _)
  }

  def execute(op: OpXmitTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.lit)))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.arg))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.reg))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmit, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitTagXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.lit)))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArgXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.arg))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitRegXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.reg))
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpSend, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'ctxt)(Ctxt.NIV)
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'xmitData)((op.unwind, op.next))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpApplyPrimTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .set(_ >> 'loc)(LocationAtom(state.code.lit(op.lit)))
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.primNum)

        val (result, newState) =
          // TODO: Remove get
          if (op.unwind) { unwindAndApplyPrim(prim.get, state) } else {
            // TODO: Fix
            (prim.get.dispatchHelper(state.ctxt), state)
          }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, newState.loc)
              newState.set(_ >> 'doNextThreadFlag)(true)
            } else {
              import Location._

              Location
                .store(newState.loc, newState.ctxt, newState.globalEnv, ob) match {
                case StoreFail => newState.set(_ >> 'vmErrorFlag)(true)

                case StoreCtxt(ctxt) =>
                  newState
                    .set(_ >> 'ctxt)(ctxt)
                    .update(_ >> 'doNextThreadFlag)(if (op.next) true else _)

                case StoreGlobal(env) => newState.set(_ >> 'globalEnv)(env)
              }
            }

          case Left(DeadThread) =>
            newState.set(_ >> 'doNextThreadFlag)(true)
        }
      })

  def execute(op: OpApplyPrimArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.primNum)
        val argno = op.arg

        val (result, newState) =
          if (op.unwind) { unwindAndApplyPrim(prim.get, state) } else {
            // TODO: Fix
            (prim.get.dispatchHelper(state.ctxt), state)
          }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, newState.loc)
              newState.set(_ >> 'doNextThreadFlag)(true)
            } else if (argno >= newState.ctxt.argvec.elem.length) {
              newState.set(_ >> 'vmErrorFlag)(true)
            } else {
              newState
                .update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
                .update(_ >> 'doNextThreadFlag)(if (op.next) true else _)
            }

          case Left(DeadThread) =>
            newState.set(_ >> 'doNextThreadFlag)(true)
        }

      })

  def execute(op: OpApplyPrimReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.primNum)
        val regno = op.reg

        val (result, newState) =
          if (op.unwind) { unwindAndApplyPrim(prim.get, state) } else {
            // TODO: Fix
            (prim.get.dispatchHelper(state.ctxt), state)
          }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, Location.CtxtReg(regno))
              newState.set(_ >> 'doNextThreadFlag)(true)
            } else {
              setCtxtReg(regno, ob)(newState)
                .update(_ >> 'doNextThreadFlag)(if (op.next) true else _)
            }

          case Left(DeadThread) =>
            newState.set(_ >> 'doNextThreadFlag)(true)
        }

      })

  def execute(op: OpApplyCmd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.nargs)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.primNum)

        val (result, newState) =
          if (op.unwind) { unwindAndApplyPrim(prim.get, state) } else {
            // TODO: Fix
            (prim.get.dispatchHelper(state.ctxt), state)
          }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, Location.LIMBO)
              newState.set(_ >> 'doNextThreadFlag)(true)
            } else {
              newState.update(_ >> 'doNextThreadFlag)(if (op.next) true else _)
            }
          case Left(DeadThread) =>
            newState.set(_ >> 'doNextThreadFlag)(true)
        }
      })

  def execute(op: OpRtn, state: VMState): VMState =
    state
      .set(_ >> 'doRtnData)(op.next)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.lit)))
      .set(_ >> 'doRtnData)(op.next)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.arg))
      .set(_ >> 'doRtnData)(op.next)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.reg))
      .set(_ >> 'doRtnData)(op.next)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpUpcallRtn, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.lit)))
      .updateSelf(state => {
        val ctxt = state.ctxt

        import Location._

        Location.store(ctxt.tag, ctxt.ctxt, state.globalEnv, ctxt.rslt) match {
          case StoreFail => state.set(_ >> 'vmErrorFlag)(true)

          case StoreCtxt(ctxt) =>
            state
              .set(_ >> 'ctxt)(ctxt)
              .update(_ >> 'doNextThreadFlag)(if (op.next) true else _)

          case StoreGlobal(env) => state.set(_ >> 'globalEnv)(env)
        }
      })

  def execute(op: OpUpcallResume, state: VMState): VMState =
    state.ctxt.ctxt
      .scheduleStrand(state)
      .set(_ >> 'doNextThreadFlag)(true)

  def execute(op: OpNxt, state: VMState): VMState = {
    val (exit, newState) = getNextStrand(state)

    if (exit) {
      newState.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(0)
    } else {
      newState
    }
  }

  def execute(op: OpJmp, state: VMState): VMState =
    state.set(_ >> 'pc >> 'relative)(op.pc)

  def execute(op: OpJmpCut, state: VMState): VMState = {
    val cut = op.cut

    val env = (1 to cut).foldLeft(state.ctxt.env)((env, _) => env.parent)

    state
      .set(_ >> 'ctxt >> 'env)(env)
      .set(_ >> 'pc >> 'relative)(op.pc)
  }

  def execute(op: OpJmpFalse, state: VMState): VMState =
    state.update(_ >> 'pc >> 'relative)(
      if (state.ctxt.rslt == Ob.RBLFALSE) op.pc else _)

  def execute(op: OpLookupToArg, state: VMState): VMState = {
    val argno = op.arg
    val key = state.code.lit(op.lit)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) =>
        handleMissingBinding(key, Location.ArgReg(argno))
        state.set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
    }
  }

  def execute(op: OpLookupToReg, state: VMState): VMState = {
    val regno = op.reg
    val key = state.code.lit(op.lit)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) =>
        handleMissingBinding(key, Location.CtxtReg(regno))
        state.set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) => setCtxtReg(regno, ob)(state)
    }
  }

  def execute(op: OpXferLexToArg, state: VMState): VMState = {
    val level = op.level

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent)

    val environment = if (op.indirect) {
      env.as[Actor].map(_.extension)
    } else {
      Some(env)
    }

    environment match {
      case Some(e) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
          _.updated(op.arg, e.slot(op.offset)))
      case None => die("OpXferLexToArg: Type mismatch")(state)
    }
  }

  def execute(op: OpXferLexToReg, state: VMState): VMState = {
    val level = op.level

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent)

    val environment = if (op.indirect) {
      env.as[Actor].map(_.extension)
    } else {
      Some(env)
    }

    environment match {
      case Some(e) =>
        setCtxtReg(op.reg, e.slot(op.offset))(state)
      case None => die("OpXferLexToReg: Type mismatch")(state)
    }

  }

  def execute(op: OpXferGlobalToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.arg, state.globalEnv.entry(op.global)))

  def execute(op: OpXferGlobalToReg, state: VMState): VMState =
    setCtxtReg(op.reg, state.globalEnv.entry(op.global))(state)

  def execute(op: OpXferArgToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.dest, state.ctxt.argvec.elem(op.src)))

  def execute(op: OpXferRsltToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.arg, state.ctxt.rslt))

  def execute(op: OpXferArgToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.argvec.elem(op.arg))

  def execute(op: OpXferRsltToReg, state: VMState): VMState =
    setCtxtReg(op.reg, state.ctxt.rslt)(state)

  def execute(op: OpXferRegToRslt, state: VMState): VMState = {
    val (obOpt, newState) = getCtxtReg(op.reg)(state)
    obOpt.map(ob => newState.set(_ >> 'ctxt >> 'rslt)(ob)).getOrElse(newState)
  }

  def execute(op: OpXferRsltToDest, state: VMState): VMState =
    state
    //.set(_ >> 'loc)(LocationAtom(state.code.lit(op.lit))) //TODO probably we should change `atom` field only
      .updateSelf(
        state => {
          import Location._

          Location.store(state.loc,
                         state.ctxt,
                         state.globalEnv,
                         state.ctxt.rslt) match {
            case StoreFail => state.set(_ >> 'vmErrorFlag)(true)

            case StoreCtxt(ctxt) => state.set(_ >> 'ctxt)(ctxt)

            case StoreGlobal(env) => state.set(_ >> 'globalEnv)(env)
          }
        })

  def execute(op: OpXferSrcToRslt, state: VMState): VMState =
    state
      .set(_ >> 'loc)(LocationAtom(state.code.lit(op.lit)))
      .set(_ >> 'ctxt >> 'rslt)(
        Location.fetch(state.loc, state.ctxt, state.globalEnv))

  def execute(op: OpIndLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.arg, state.code.lit(op.lit)))

  def execute(op: OpIndLitToReg, state: VMState): VMState =
    setCtxtReg(op.reg, state.code.lit(op.lit))(state)

  def execute(op: OpIndLitToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.code.lit(op.lit))

  def execute(op: OpImmediateLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.arg, vmLiterals(op.value)))

  def execute(op: OpImmediateLitToReg, state: VMState): VMState =
    setCtxtReg(op.reg, vmLiterals(op.lit))(state)

  def execute(op: OpUnknown, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(1)
}
