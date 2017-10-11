package coop.rchain.rosette

import com.typesafe.scalalogging.Logger

sealed trait RblError
case object DeadThread extends RblError

sealed trait Work
case object NoWorkLeft extends Work
case object WaitForAsync extends Work
case class StrandsScheduled(state: VMState) extends Work

object VirtualMachine {

  val logger = Logger("opcode")

  def unwindAndApplyPrim(prim: Prim): Either[RblError, Ob] = Right(null)
  def handleException(result: Ob, op: Op, loc: Location): Ob = null
  def handleFormalsMismatch(formals: Template): Ob = null
  def handleMissingBinding(key: Ob, argReg: Location): Ob = null

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

  def getNextStrand(state: VMState): (Boolean, VMState) =
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
    var pc = 0
    var exit = false
    var currentState = state

    while (pc < opCodes.size && !exit) {
      val op = opCodes(pc)
      logger.info("PC: " + pc + " Opcode: " + op)

      currentState = modifyFlags(executeDispatch(op, currentState))
        .update(_ >> 'bytecodes)(
          _.updated(op, currentState.bytecodes.getOrElse(op, 0.toLong) + 1))
        .update(_ >> 'pc >> 'relative)(_ + 1)

      pc = currentState.pc.relative

      if (currentState.exitFlag) exit = true
    }

    currentState
  }

  def modifyFlags(state: VMState): VMState = {
    var mState = state

    if (mState.doXmitFlag) {
      // may set doNextThreadFlag
    }

    if (mState.doRtnFlag) {
      //if (mState.ctxt.ret(mState.ctxt.rslt)) {
      //  mState = mState.set(_ >> 'vmErrorFlag)(true)
      //} else if (mState.doRtnFlag) {
      //  mState = mState.set(_ >> 'doNextThreadFlag)(true)
      //}
    }

    if (mState.vmErrorFlag) {
      //handleVirtualMachineError()
      mState = mState.set(_ >> 'doNextThreadFlag)(true)
    }

    if (mState.doNextThreadFlag) {
      //if (getNextStrand()) {
      //  tmpState = tmpState.set(_ >> 'nextOpFlag)(false)
      //}
    }

    mState
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
    state.set(_ >> 'ctxt >> 'nargs)(op.n)

  def execute(op: OpAlloc, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'argvec)(Tuple(op.n, None))

  def execute(op: OpPushAlloc, state: VMState): VMState =
    state.update(_ >> 'ctxt)(Ctxt(Some(Tuple(op.n, None)), _))

  def execute(op: OpExtend, state: VMState): VMState = {
    val formals = state.code.lit(op.v).asInstanceOf[Template]
    val actuals = formals.matchPattern(state.ctxt.argvec, state.ctxt.nargs)

    actuals match {
      case Some(tuple) =>
        state
          .set(_ >> 'ctxt >> 'nargs)(0)
          .set(_ >> 'ctxt >> 'env)(
            state.ctxt.env.extendWith(formals.keymeta, tuple))

      case None =>
        handleFormalsMismatch(formals)
        state.set(_ >> 'doNextThreadFlag)(true)
    }
  }

  def execute(op: OpOutstanding, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'pc)(PC.fromInt(op.p))
      .set(_ >> 'ctxt >> 'outstanding)(op.n)

  def execute(op: OpFork, state: VMState): VMState =
    state.set(_ >> 'strandPool)(
      state.ctxt.copy(pc = PC.fromInt(op.p)) +: state.strandPool)

  def execute(op: OpXmitTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.v)))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmit, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitTagXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.v)))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArgXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitRegXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpSend, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'ctxt)(Ctxt.NIV)
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpApplyPrimTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'loc)(LocationAtom(state.code.lit(op.v)))
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.k)
        val result = if (op.u) { unwindAndApplyPrim(prim) } else {
          prim.dispatchHelper(state.ctxt)
        }
        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, state.loc)
              state.set(_ >> 'doNextThreadFlag)(true)
            } else {
              import Location._

              Location
                .store(state.loc, state.ctxt, state.globalEnv, ob) match {
                case StoreFail => state.set(_ >> 'vmErrorFlag)(true)

                case StoreCtxt(ctxt) =>
                  state
                    .set(_ >> 'ctxt)(ctxt)
                    .update(_ >> 'doNextThreadFlag)(if (op.n) true else _)

                case StoreGlobal(env) => state.set(_ >> 'globalEnv)(env)
              }
            }

          case Left(DeadThread) =>
            state.set(_ >> 'doNextThreadFlag)(true)
        }
      })

  def execute(op: OpApplyPrimArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.k)
        val argno = op.a
        val result = if (op.u) { unwindAndApplyPrim(prim) } else {
          prim.dispatchHelper(state.ctxt)
        }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, state.loc)
              state.set(_ >> 'doNextThreadFlag)(true)
            } else if (argno >= state.ctxt.argvec.elem.length) {
              state.set(_ >> 'vmErrorFlag)(true)
            } else {
              state
                .update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
                .update(_ >> 'doNextThreadFlag)(if (op.n) true else _)
            }

          case Left(DeadThread) =>
            state.set(_ >> 'doNextThreadFlag)(true)
        }

      })

  def execute(op: OpApplyPrimReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.k)
        val regno = op.r
        val result = if (op.u) { unwindAndApplyPrim(prim) } else {
          prim.dispatchHelper(state.ctxt)
        }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, Location.CtxtReg(regno))
              state.set(_ >> 'doNextThreadFlag)(true)
            } else {
              state
                .update(_ >> 'ctxt >> 'reg)(_.updated(regno, ob))
                .update(_ >> 'doNextThreadFlag)(if (op.n) true else _)
            }

          case Left(DeadThread) =>
            state.set(_ >> 'doNextThreadFlag)(true)
        }

      })

  def execute(op: OpApplyCmd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .updateSelf(state => {
        val prim = Prim.nthPrim(op.k)
        val result = if (op.u) { unwindAndApplyPrim(prim) } else {
          prim.dispatchHelper(state.ctxt)
        }

        result match {
          case Right(ob) =>
            if (ob.is(Ob.OTsysval)) {
              handleException(ob, op, Location.LIMBO)
              state.set(_ >> 'doNextThreadFlag)(true)
            } else {
              state.update(_ >> 'doNextThreadFlag)(if (op.n) true else _)
            }
          case Left(DeadThread) =>
            state.set(_ >> 'doNextThreadFlag)(true)
        }
      })

  def execute(op: OpRtn, state: VMState): VMState =
    state
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.v)))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpUpcallRtn, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(LocationAtom(state.code.lit(op.v)))
      .updateSelf(state => {
        val ctxt = state.ctxt

        import Location._

        Location.store(ctxt.tag, ctxt.ctxt, state.globalEnv, ctxt.rslt) match {
          case StoreFail => state.set(_ >> 'vmErrorFlag)(true)

          case StoreCtxt(ctxt) =>
            state
              .set(_ >> 'ctxt)(ctxt)
              .update(_ >> 'doNextThreadFlag)(if (op.n) true else _)

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
    state.set(_ >> 'pc >> 'relative)(op.n)

  def execute(op: OpJmpCut, state: VMState): VMState = {
    val cut = op.m

    val env = (1 to cut).foldLeft(state.ctxt.env)((env, _) => env.parent)

    state
      .set(_ >> 'ctxt >> 'env)(env)
      .set(_ >> 'pc >> 'relative)(op.n)
  }

  def execute(op: OpJmpFalse, state: VMState): VMState =
    state.update(_ >> 'pc >> 'relative)(
      if (state.ctxt.rslt == Ob.RBLFALSE) op.n else _)

  def execute(op: OpLookupToArg, state: VMState): VMState = {
    val argno = op.a
    val key = state.code.lit(op.v)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) => {
        handleMissingBinding(key, Location.ArgReg(argno))
        state.set(_ >> 'doNextThreadFlag)(true)
      }

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
    }
  }

  def execute(op: OpLookupToReg, state: VMState): VMState = {
    val regno = op.r
    val key = state.code.lit(op.v)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state.set(_ >> 'doNextThreadFlag)(true)

      case Left(Absent) => {
        handleMissingBinding(key, Location.CtxtReg(regno))
        state.set(_ >> 'doNextThreadFlag)(true)
      }

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'reg)(_.updated(regno, ob))
    }
  }

  def execute(op: OpXferLexToArg, state: VMState): VMState = {
    val level = op.l

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent)

    val slot = if (op.i) {
      val actor = Actor(env)
      actor.extension.slot
    } else {
      env.slot
    }

    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.a, slot(op.o)))
  }

  def execute(op: OpXferLexToReg, state: VMState): VMState = {
    val level = op.l

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent)

    val slot = if (op.i) {
      val actor = Actor(env)
      actor.extension.slot
    } else {
      env.slot
    }

    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, slot(op.o)))
  }

  def execute(op: OpXferGlobalToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.globalEnv.entry(op.g)))

  def execute(op: OpXferGlobalToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(
      _.updated(op.r, state.globalEnv.entry(op.g)))

  def execute(op: OpXferArgToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.d, state.ctxt.argvec.elem(op.s)))

  def execute(op: OpXferRsltToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.ctxt.rslt))

  def execute(op: OpXferArgToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.argvec.elem(op.a))

  def execute(op: OpXferRsltToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, state.ctxt.rslt))

  def execute(op: OpXferRegToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.reg(op.r))

  def execute(op: OpXferRsltToDest, state: VMState): VMState =
    state
      .set(_ >> 'loc)(LocationAtom(state.code.lit(op.v)))
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
      .set(_ >> 'loc)(LocationAtom(state.code.lit(op.v)))
      .set(_ >> 'ctxt >> 'rslt)(
        Location.fetch(state.loc, state.ctxt, state.globalEnv))

  def execute(op: OpIndLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.code.lit(op.v)))

  def execute(op: OpIndLitToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, state.code.lit(op.v)))

  def execute(op: OpIndLitToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.code.lit(op.v))

  def execute(op: OpImmediateLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, vmLiterals(op.v)))

  def execute(op: OpImmediateLitToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, vmLiterals(op.v)))

  def execute(op: OpUnknown, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true).set(_ >> 'exitCode)(1)
}
