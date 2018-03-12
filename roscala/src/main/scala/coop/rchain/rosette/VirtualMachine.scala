package coop.rchain.rosette

import cats.data.State
import cats.data.State._
import com.typesafe.scalalogging.Logger
import coop.rchain.rosette.Ob._
import coop.rchain.rosette.Ctxt.{setReg, Continuation, CtxtTransition}
import coop.rchain.rosette.prim.Prim

sealed trait Work
case object NoWorkLeft                      extends Work
case object WaitForAsync                    extends Work
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

  val doNextThread = modify[VMState](_.copy(doNextThreadFlag = true))
  val doNothing    = pure[VMState, Unit](())
  val vmError      = modify[VMState](_.copy(exitFlag = true, exitCode = 1))

  def handleApplyPrimSuspend(op: Op): Unit                = ()
  def handleApplyPrimUpcall(op: Op, tag: Location): Unit  = ()
  def handleFormalsMismatch(formals: Template): Ob        = null
  def handleMissingBinding(key: Ob, argReg: Location): Ob = null
  def handleSleep(): Unit                                 = ()
  def handleXmitUpcall(op: Op, tag: Location): Unit       = ()

  /*
  def handleVirtualMachineError(state: VMState): VMState =
    state.ctxt.vmError(state)._2
   */

  def handlePrimResult(primResult: Result, save: Ob => VMTransition[Unit]): VMTransition[Unit] =
    primResult match {
      case Right(ob) => save(ob)

      case Left(DeadThread) => doNextThread

      case Left(PrimNotFound) => vmError

      // TODO: Port handleException for OTsysval
      case Left(_) => doNextThread
    }

  def runPrim(unwind: Boolean, optPrim: Option[Prim]): VMTransition[Result] =
    for {
      result <- optPrim match {
                 case Some(prim) if unwind  => unwindAndApplyPrim(prim).embedCtxt
                 case Some(prim) if !unwind => prim.dispatchHelper.embedCtxt
                 case None                  => pure[VMState, Result](Left(PrimNotFound))
               }
    } yield result

  // TODO: Finish
  def unwindAndApplyPrim(prim: Prim): CtxtTransition[Result] = {
    val unwind: CtxtTransition[Unit]         = ???
    val recoil: Ctxt => CtxtTransition[Unit] = ???

    for {
      oldCtxt    <- get[Ctxt]
      _          <- unwind
      primResult <- prim.dispatchHelper
      _          <- recoil(oldCtxt)
    } yield primResult
  }

  def handleException(v: Ob, op: Op, tag: Location): Unit =
    v.sysval match {
      case SyscodeUpcall =>
        op match {
          case _: OpApplyCmd | _: OpApplyPrimArg | _: OpApplyPrimReg | _: OpApplyPrimTag =>
            handleApplyPrimUpcall(op, tag)
          case _ =>
            handleXmitUpcall(op, tag)
        }

      case SyscodeSuspend =>
        op match {
          case _: OpApplyCmd | _: OpApplyPrimArg | _: OpApplyPrimReg | _: OpApplyPrimTag =>
            handleApplyPrimSuspend(op)

          case _ => // Nothing happens; this is the usual case.
        }

      case SyscodeInterrupt => suicide("what to do with syscodeInterrupt?")

      case SyscodeSleep => handleSleep()

      case SyscodeInvalid | SyscodeDeadThread => // We don't do diddly

      case _ => suicide(s"unknown SysCode value (${v.sysval})")
    }

  def getNextStrand: VMTransition[Boolean] = State { state =>
    loggerStrand.info("Try to get next ctxt")
    if (state.strandPool.isEmpty)
      loggerStrand.info("strandPool is empty")
    else
      loggerStrand.info(
        s"strandPool contains ${state.strandPool.size} ctxt: ${state.strandPool.map(_.id).mkString(", ")}")

    if (state.strandPool.isEmpty) {
      tryAwakeSleepingStrand(state) match {
        case WaitForAsync =>
          val newState = state.set(_ >> 'doAsyncWaitFlag)(true)
          (newState, false)

        case NoWorkLeft => (state, true)

        case StrandsScheduled(stateScheduled) =>
          val stateDebug =
            if (stateScheduled.debug)
              state.update(_ >> 'debugInfo)(_ :+ "*** waking sleepers\n")
            else
              stateScheduled

          val strand   = stateDebug.strandPool.head
          val newState = stateDebug.update(_ >> 'strandPool)(_.tail)

          (installStrand(strand, newState), false)
      }
    } else {
      val strand   = state.strandPool.head
      val newState = state.update(_ >> 'strandPool)(_.tail)

      (installStrand(strand, newState), false)
    }
  }

  def tryAwakeSleepingStrand(state: VMState): Work =
    if (state.sleeperPool.isEmpty) {
      if (state.nsigs == 0)
        NoWorkLeft
      else
        WaitForAsync
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

    installCtxt(strand, stateInstallMonitor)
  }

  def installMonitor(monitor: Monitor, state: VMState): VMState = {
    val stateDebug =
      if (state.debug)
        state.update(_ >> 'debugInfo)(_ :+ s"*** new monitor: ${monitor.id}\n")
      else
        state

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
    val stateDebug =
      if (state.debug)
        state.update(_ >> 'debugInfo)(_ :+ "*** new strand\n")
      else
        state

    loggerStrand.info(s"Install ctxt ${ctxt.id}")
    //loggerStrand.info(s"Install code with size ${ctxt.code.codevec.size}")
    //loggerStrand.info(s"Install pc value ${ctxt.pc.relative}")

    stateDebug
      .set(_ >> 'ctxt)(ctxt)
      .set(_ >> 'code)(ctxt.code)
      .set(_ >> 'pc >> 'relative)(ctxt.pc.relative)
  }

  def executeSeq(opcodes: Seq[Op], state: VMState): VMState = {
    var pc           = state.pc.relative
    var exit         = false
    var currentState = state
    var codevec      = opcodes

    loggerStrand.info(s"Current ctxt: ${state.ctxt.id} | Parent ctxt: ${state.ctxt.ctxt.id}")

    while (pc < codevec.size && !exit) {
      val op = codevec(pc)
      loggerOpcode.info("PC: " + pc + " Opcode: " + op)

      val tmpState = for {
        _ <- modify[VMState](
              _.update(_ >> 'pc >> 'relative)(_ + 1)
                .update(_ >> 'bytecodes)(
                  _.updated(op, currentState.bytecodes.getOrElse(op, 0.toLong) + 1)))

        _ <- executeDispatch(op)
        _ <- runFlags
      } yield ()

      currentState = tmpState.runS(currentState).value

      pc = currentState.pc.relative
      codevec = currentState.code.codevec

      if (currentState.exitFlag) exit = true
    }

    loggerOpcode.info("Exiting")
    currentState
  }

  // TODO: Use state monad here
  def runFlags: VMTransition[Unit] = modify { state =>
    var mState = state

    if (mState.doXmitFlag) {
      // may set doNextThreadFlag
      mState = doXmit.runS(mState).value.set(_ >> 'doXmitFlag)(false)
    }

    if (mState.doRtnFlag) {
      // may set doNextThreadFlag
      mState = doRtn.runS(mState).value.set(_ >> 'doRtnFlag)(false)
    }

    if (mState.vmErrorFlag) {
      // TODO: Revisit once OprnVmError works
      //handleVirtualMachineError(mState)
      mState = mState.set(_ >> 'doNextThreadFlag)(true)
    }

    if (mState.doNextThreadFlag) {
      val (tmpState, isEmpty) = getNextStrand.run(mState).value
      loggerStrand.info("Current codesize: " + mState.code.codevec.size)
      loggerStrand.info("Current pc: " + mState.pc.relative)
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

  def setCtxtReg(reg: Int, ob: Ob)(state: VMState): VMState = {
    val (ctxt, storeRes) = setReg(reg, ob).run(state.ctxt).value

    storeRes match {
      case Success => state.set(_ >> 'ctxt)(ctxt)
      case Failure => die(unknownRegister(reg))(state)
    }
  }

  def getCtxtReg(reg: Int): VMTransition[Option[Ob]] = State { state =>
    state.ctxt.getReg(reg) match {
      case someOb @ Some(_) => (state, someOb)
      case None             => (die(unknownRegister(reg))(state), None)
    }
  }

  def schedule(ctxt: Ctxt): VMTransition[Unit] = {
    loggerStrand.info("Schedule ctxt " + ctxt.id)
    modify[VMState](_.update(_ >> 'strandPool)(_ :+ ctxt))
  }

  /** Return current result
    *
    * This returns the current result to the parent ctxt.
    * The parent ctxt possibly gets scheduled.
    */
  def doRtn: VMTransition[Unit] =
    for {
      rslt        <- inspect[VMState, Ob](_.ctxt.rslt)
      ctxtRet     <- Ctxt.ret(rslt).embedCtxt
      isDoRtnFlag <- inspect[VMState, Boolean](_.doRtnFlag)

      (isError, optContinuation) = ctxtRet

      _ <- optContinuation.map(schedule).getOrElse(pure(()))

      _ <- if (isError)
            modify[VMState](_.copy(vmErrorFlag = true))
          else
            doNothing

      _ <- if (isDoRtnFlag) doNextThread else doNothing
    } yield ()

  /** Dispatch current target
    *
    * This puts the result of the dispatch into the parent ctxt.
    * The parent ctxt (which includes the result) possibly gets scheduled.
    *
    * TODO: Add unwind
    */
  def doXmit: VMTransition[Unit] =
    for {
      target         <- inspect[VMState, Ob](_.ctxt.trgt)
      dispatchResult <- target.dispatch.embedCtxt
      next           <- inspect[VMState, Boolean](_.xmitData._2)

      (result, optContinuation) = dispatchResult

      _ <- optContinuation.map(schedule).getOrElse(pure(()))

      _ <- result match {
            // TODO: Add missing case where result is OTsysval
            case Left(DeadThread) => doNextThread

            // This is the usual case
            case Left(Suspended) => doNothing

            case _ if next => doNextThread

            case _ => doNothing
          }
    } yield ()

  def executeDispatch(op: Op): VMTransition[Unit] =
    op match {
      case o: OpHalt              => execute(o)
      case o: OpPush              => execute(o)
      case o: OpPop               => execute(o)
      case o: OpNargs             => execute(o)
      case o: OpPushAlloc         => execute(o)
      case o: OpExtend            => execute(o)
      case o: OpOutstanding       => execute(o)
      case o: OpAlloc             => execute(o)
      case o: OpFork              => execute(o)
      case o: OpXmitTag           => execute(o)
      case o: OpXmitArg           => execute(o)
      case o: OpXmitReg           => execute(o)
      case o: OpXmit              => execute(o)
      case o: OpXmitTagXtnd       => execute(o)
      case o: OpXmitArgXtnd       => execute(o)
      case o: OpXmitRegXtnd       => execute(o)
      case o: OpSend              => execute(o)
      case o: OpApplyPrimTag      => execute(o)
      case o: OpApplyPrimArg      => execute(o)
      case o: OpApplyPrimReg      => execute(o)
      case o: OpApplyCmd          => execute(o)
      case o: OpRtnTag            => execute(o)
      case o: OpRtnArg            => execute(o)
      case o: OpRtnReg            => execute(o)
      case o: OpRtn               => execute(o)
      case o: OpUpcallRtn         => execute(o)
      case o: OpUpcallResume      => execute(o)
      case o: OpNxt               => execute(o)
      case o: OpJmp               => execute(o)
      case o: OpJmpFalse          => execute(o)
      case o: OpJmpCut            => execute(o)
      case o: OpLookupToArg       => execute(o)
      case o: OpLookupToReg       => execute(o)
      case o: OpXferLexToArg      => execute(o)
      case o: OpXferLexToReg      => execute(o)
      case o: OpXferGlobalToArg   => execute(o)
      case o: OpXferGlobalToReg   => execute(o)
      case o: OpXferArgToArg      => execute(o)
      case o: OpXferRsltToArg     => execute(o)
      case o: OpXferArgToRslt     => execute(o)
      case o: OpXferRsltToReg     => execute(o)
      case o: OpXferRegToRslt     => execute(o)
      case o: OpXferRsltToDest    => execute(o)
      case o: OpXferSrcToRslt     => execute(o)
      case o: OpIndLitToArg       => execute(o)
      case o: OpIndLitToReg       => execute(o)
      case o: OpIndLitToRslt      => execute(o)
      case o: OpImmediateLitToArg => execute(o)
      case o: OpImmediateLitToReg => execute(o)
      case o: OpUnknown           => execute(o)
    }

  def execute(op: OpHalt): VMTransition[Unit] =
    modify(_.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(0))

  def execute(op: OpPush): VMTransition[Unit] =
    modify(state => state.set(_ >> 'ctxt)(Ctxt(Tuple.NIL, state.ctxt)))

  def execute(op: OpPop): VMTransition[Unit] =
    modify(state => state.set(_ >> 'ctxt)(state.ctxt.ctxt))

  def execute(op: OpNargs): VMTransition[Unit] = modify(_.set(_ >> 'ctxt >> 'nargs)(op.nargs))

  def execute(op: OpAlloc): VMTransition[Unit] =
    modify(_.set(_ >> 'ctxt >> 'argvec)(Tuple(op.n, NIV)))

  def execute(op: OpPushAlloc): VMTransition[Unit] =
    modify(_.update(_ >> 'ctxt)(Ctxt(Tuple(op.n, NIV), _)))

  def execute(op: OpExtend): VMTransition[Unit] = modify { state =>
    def getTemplate = state.code.lit(op.lit).as[Template]
    def matchActuals(template: Template) =
      template.`match`(state.ctxt.argvec, state.ctxt.nargs)
    def getStdExtension = state.ctxt.env.as[StdExtension]

    val stateOrDie = for {
      template <- getTemplate or die(s"OpExtend: No template in state.code.litvec(${op.lit})")(
                   state)

      tuple <- matchActuals(template) or
                // TODO: Revisit
                //handleFormalsMismatch(formals)
                state.set(_ >> 'doNextThreadFlag)(true)

      env <- getStdExtension or die("OpExtend: state.ctxt.env needs to be a StdExtension")(state)

    } yield {
      val newEnv = env.extendWith(template.keyMeta, tuple)
      state.set(_ >> 'ctxt >> 'env)(newEnv).set(_ >> 'ctxt >> 'nargs)(0)
    }

    stateOrDie.merge
  }

  def execute(op: OpOutstanding): VMTransition[Unit] =
    modify(_.set(_ >> 'ctxt >> 'pc)(PC(op.pc)).set(_ >> 'ctxt >> 'outstanding)(op.n))

  def execute(op: OpFork): VMTransition[Unit] =
    for {
      ctxt <- inspect[VMState, Ctxt](_.ctxt.copy(pc = PC(op.pc)))
      _    <- modify[VMState](_.update(_ >> 'strandPool)(ctxt +: _))
    } yield ()

  def execute(op: OpXmitTag): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'nargs)(op.nargs)
          .set(_ >> 'ctxt >> 'tag)(state.code.lit(op.lit).asInstanceOf[Location])
          .set(_ >> 'xmitData)((op.unwind, op.next))
          .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpXmitArg): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'nargs)(op.nargs)
          .set(_ >> 'ctxt >> 'tag)(ArgRegister(op.arg))
          .set(_ >> 'xmitData)((op.unwind, op.next))
          .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpXmitReg): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'nargs)(op.nargs)
          .set(_ >> 'ctxt >> 'tag)(CtxtRegister(op.reg))
          .set(_ >> 'xmitData)((op.unwind, op.next))
          .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpXmit): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'nargs)(op.nargs)
          .set(_ >> 'xmitData)((op.unwind, op.next))
          .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpXmitTagXtnd): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'nargs)(op.nargs)
          .set(_ >> 'ctxt >> 'tag)(state.code.lit(op.lit).asInstanceOf[Location])
          .set(_ >> 'xmitData)((op.unwind, op.next))
          .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpXmitArgXtnd): VMTransition[Unit] =
    execute(OpXmitArg(op.unwind, op.next, op.nargs, op.arg))

  def execute(op: OpXmitRegXtnd): VMTransition[Unit] =
    execute(OpXmitReg(op.unwind, op.next, op.nargs, op.reg))

  def execute(op: OpSend): VMTransition[Unit] =
    modify(
      _.set(_ >> 'ctxt >> 'ctxt)(Ctxt.NIV)
        .set(_ >> 'ctxt >> 'nargs)(op.nargs)
        .set(_ >> 'xmitData)((op.unwind, op.next))
        .set(_ >> 'doXmitFlag)(true))

  def execute(op: OpApplyPrimTag): VMTransition[Unit] =
    for {
      loc <- inspect[VMState, Location](_.code.lit(op.lit).asInstanceOf[Location])
      _   <- modify[VMState](_.set(_ >> 'ctxt >> 'nargs)(op.nargs))
      _   <- modify[VMState](_.set(_ >> 'loc)(loc))

      prim   = Prim.nthPrim(op.primNum)
      result <- runPrim(op.unwind, prim)

      _ <- handlePrimResult(
            result,
            ob =>
              Location
                .store(loc, ob)
                .transformS[VMState](_.ctxt, (vmState, ctxt) => vmState.copy(ctxt = ctxt))
                .transform { (vmState, storeResult) =>
                  storeResult match {
                    case Success =>
                      (vmState.update(_ >> 'doNextThreadFlag)(op.next || _), ())
                    case Failure =>
                      (vmState.copy(exitFlag = true, exitCode = 1), ())
                  }
              }
          )

    } yield ()

  def execute(op: OpApplyPrimArg): VMTransition[Unit] =
    for {
      _ <- modify[VMState](_.set(_ >> 'ctxt >> 'nargs)(op.nargs))

      prim   = Prim.nthPrim(op.primNum)
      result <- runPrim(op.unwind, prim)

      _ <- handlePrimResult(
            result,
            ob =>
              State { vmState: VMState =>
                if (ob.is(Ob.OTsysval))
                  //handleException(ob, op, vmState.loc)
                  (vmState.copy(doNextThreadFlag = true), ())
                else if (op.arg >= vmState.ctxt.argvec.elem.size)
                  (vmState.copy(vmErrorFlag = true), ())
                else
                  (vmState
                     .update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.arg, ob))
                     .update(_ >> 'doNextThreadFlag)(op.next || _),
                   ())
            }
          )
    } yield ()

  def execute(op: OpApplyPrimReg): VMTransition[Unit] =
    for {
      _ <- modify[VMState](_.set(_ >> 'ctxt >> 'nargs)(op.nargs))

      prim   = Prim.nthPrim(op.primNum)
      result <- runPrim(op.unwind, prim)

      _ <- handlePrimResult(
            result,
            ob =>
              setReg(op.reg, ob).embedCtxt
                .transform { (vmState, storeResult) =>
                  storeResult match {
                    case Success =>
                      (vmState.update(_ >> 'doNextThreadFlag)(op.next || _), ())
                    case Failure =>
                      (vmState.copy(exitFlag = true, exitCode = 1), ())
                  }
              }
          )
    } yield ()

  def execute(op: OpApplyCmd): VMTransition[Unit] =
    for {
      _ <- modify[VMState](_.set(_ >> 'ctxt >> 'nargs)(op.nargs))

      prim   = Prim.nthPrim(op.primNum)
      result <- runPrim(op.unwind, prim)

      _ <- handlePrimResult(
            result,
            ob => modify(_.copy(doNextThreadFlag = true))
          )
    } yield ()

  def execute(op: OpRtn): VMTransition[Unit] =
    modify(_.set(_ >> 'doRtnData)(op.next).set(_ >> 'doRtnFlag)(true))

  def execute(op: OpRtnTag): VMTransition[Unit] =
    modify(
      state =>
        state
          .set(_ >> 'ctxt >> 'tag)(state.code.lit(op.lit).asInstanceOf[Location])
          .set(_ >> 'doRtnData)(op.next)
          .set(_ >> 'doRtnFlag)(true))

  def execute(op: OpRtnArg): VMTransition[Unit] =
    modify(
      _.set(_ >> 'ctxt >> 'tag)(ArgRegister(op.arg))
        .set(_ >> 'doRtnData)(op.next)
        .set(_ >> 'doRtnFlag)(true))

  def execute(op: OpRtnReg): VMTransition[Unit] =
    modify(
      _.set(_ >> 'ctxt >> 'tag)(CtxtRegister(op.reg))
        .set(_ >> 'doRtnData)(op.next)
        .set(_ >> 'doRtnFlag)(true))

  def execute(op: OpUpcallRtn): VMTransition[Unit] =
    for {
      result   <- inspect[VMState, Ob](_.ctxt.rslt)
      location <- inspect[VMState, Location](_.code.lit(op.lit).asInstanceOf[Location])

      _ <- modify[VMState](_.set(_ >> 'ctxt >> 'tag)(location))

      storeResult <- Location
                      .store(location, result)
                      .transformS[VMState](
                        _.ctxt.ctxt,
                        (vmState, ctxt) => vmState.set(_ >> 'ctxt >> 'ctxt)(ctxt))

      _ <- storeResult match {
            case Success if op.next => doNextThread
            case Success            => execute(OpUpcallResume())
            case Failure            => vmError
          }
    } yield ()

  def execute(op: OpUpcallResume): VMTransition[Unit] =
    for {
      parentCtxt <- inspect[VMState, Ctxt](_.ctxt.ctxt)
      _          <- schedule(parentCtxt)
      _          <- doNextThread
    } yield ()

  def execute(op: OpNxt): VMTransition[Unit] =
    for {
      exit <- getNextStrand
      _ <- if (exit)
            modify[VMState](_.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(0))
          else
            pure[VMState, Unit](())
    } yield ()

  def execute(op: OpJmp): VMTransition[Unit] = modify(_.set(_ >> 'pc >> 'relative)(op.pc))

  def execute(op: OpJmpCut): VMTransition[Unit] = modify { state =>
    val cut = op.cut

    val env = (1 to cut).foldLeft(state.ctxt.env)((env, _) => env.parent)

    state
      .set(_ >> 'ctxt >> 'env)(env)
      .set(_ >> 'pc >> 'relative)(op.pc)
  }

  def execute(op: OpJmpFalse): VMTransition[Unit] =
    modify(state =>
      state.update(_ >> 'pc >> 'relative)(if (state.ctxt.rslt == Ob.RBLFALSE) op.pc else _))

  def execute(op: OpLookupToArg): VMTransition[Unit] = modify { state =>
    val argno = op.arg
    val key   = state.code.lit(op.lit)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) =>
        handleMissingBinding(key, ArgRegister(argno))
        state.set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
    }
  }

  def execute(op: OpLookupToReg): VMTransition[Unit] = modify { state =>
    val regno = op.reg
    val key   = state.code.lit(op.lit)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) =>
        handleMissingBinding(key, CtxtRegister(regno))
        state.set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) => setCtxtReg(regno, ob)(state)
    }
  }

  def execute(op: OpXferLexToArg): VMTransition[Unit] = modify { state =>
    val level = op.level

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent)

    val environment = if (op.indirect) {
      env.as[Actor].map(_.extension)
    } else {
      Some(env)
    }

    environment match {
      case Some(e) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.arg, e.slot(op.offset)))
      case None => die("OpXferLexToArg: Type mismatch")(state)
    }
  }

  def execute(op: OpXferLexToReg): VMTransition[Unit] = modify { state =>
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

  def execute(op: OpXferGlobalToArg): VMTransition[Unit] =
    modify(
      state =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
          _.updated(op.arg, state.globalEnv.entry(op.global))))

  def execute(op: OpXferGlobalToReg): VMTransition[Unit] =
    modify(state => setCtxtReg(op.reg, state.globalEnv.entry(op.global))(state))

  def execute(op: OpXferArgToArg): VMTransition[Unit] =
    modify(
      state =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
          _.updated(op.dest, state.ctxt.argvec.elem(op.src))))

  def execute(op: OpXferRsltToArg): VMTransition[Unit] =
    modify(
      state => state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.arg, state.ctxt.rslt)))

  def execute(op: OpXferArgToRslt): VMTransition[Unit] =
    modify(state => state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.argvec.elem(op.arg)))

  def execute(op: OpXferRsltToReg): VMTransition[Unit] =
    modify(state => setCtxtReg(op.reg, state.ctxt.rslt)(state))

  def execute(op: OpXferRegToRslt): VMTransition[Unit] =
    for {
      optOb <- getCtxtReg(op.reg)
      _ <- optOb match {
            case Some(ob) => modify[VMState](_.set(_ >> 'ctxt >> 'rslt)(ob))
            case None     => pure[VMState, Unit](())
          }
    } yield ()

  def execute(op: OpXferRsltToDest): VMTransition[Unit] =
    for {
      location <- inspect[VMState, Location](_.code.lit(op.lit).asInstanceOf[Location])
      _        <- modify[VMState](_.copy(loc = location))
      rslt     <- inspect[VMState, Ob](_.ctxt.rslt)

      _ <- Location
            .store(location, rslt)
            .embedCtxt
            .transform { (vmState, storeRes) =>
              storeRes match {
                case Success => (vmState, ())
                case Failure => (vmState.copy(vmErrorFlag = true), ())
              }
            }
    } yield ()

  def execute(op: OpXferSrcToRslt): VMTransition[Unit] =
    for {
      location  <- inspect[VMState, Location](_.code.lit(op.lit).asInstanceOf[Location])
      globalEnv <- inspect[VMState, TblObject](_.globalEnv)
      _         <- modify[VMState](_.copy(loc = location))
      _ <- Location
            .fetch(location, globalEnv)
            .transform((ctxt, optRes) => (ctxt.copy(rslt = optRes.getOrElse(Ob.INVALID)), ()))
            .transformS[VMState](_.ctxt, (vmState, ctxt) => vmState.copy(ctxt = ctxt))
    } yield ()

  def execute(op: OpIndLitToArg): VMTransition[Unit] =
    modify(state =>
      state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.arg, state.code.lit(op.lit))))

  def execute(op: OpIndLitToReg): VMTransition[Unit] =
    modify(state => setCtxtReg(op.reg, state.code.lit(op.lit))(state))

  def execute(op: OpIndLitToRslt): VMTransition[Unit] =
    modify(state => state.set(_ >> 'ctxt >> 'rslt)(state.code.lit(op.lit)))

  def execute(op: OpImmediateLitToArg): VMTransition[Unit] =
    modify(state =>
      state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.arg, vmLiterals(op.value))))

  def execute(op: OpImmediateLitToReg): VMTransition[Unit] =
    modify(state => setCtxtReg(op.reg, vmLiterals(op.lit))(state))

  def execute(op: OpUnknown): VMTransition[Unit] =
    modify(_.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(1))
}
