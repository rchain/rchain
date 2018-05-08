package coop.rchain.rosette

import coop.rchain.rosette.VirtualMachine.loggerStrand
import Location._
import cats.Eval
import cats.data.State
import cats.implicits._
import coop.rchain.rosette.Ob.Lenses._

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
                monitor: Monitor, // reg[9]
                id: Int = 100) // debugging purposes only
    extends Ob {

  override val meta   = null
  override val parent = null

  private val regs =
    Vector(rslt, trgt, argvec, env, code, ctxt, self2, selfEnv, rcvr, monitor)

  def arg(n: Int): Option[Ob] = argvec.elem.lift(n)

  def getReg(r: Int): Option[Ob] = regs.lift(r)

  /** This is necessary because the compiler sometimes arranges to
    *  provide an argvec that is acually longer than nargs indicates. If
    *  we are about to expose the context to the outside world, we need
    *  to clean it up so that it appears consistent.
    */
  def prepare(): Ctxt = this.copy(argvec = argvec.makeSlice(0, nargs))

  def scheduleStrand(state: VMState): VMState = {
    loggerStrand.info(s"Schedule strand ${state.ctxt.ctxt.id}")
    state.update(_ >> 'strandPool)(_ :+ state.ctxt.ctxt)
  }
}

object Ctxt {
  type Continuation = Ctxt

  val empty = Ctxt(
    tag = Limbo,
    nargs = 0,
    outstanding = 0,
    pc = PC(0),
    rslt = null,
    trgt = null,
    argvec = Tuple.NIL,
    env = null,
    ctxt = null,
    code = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  def apply(tuple: Tuple, ctxt: Ctxt): Ctxt = Ctxt(
    tag = LocRslt,
    nargs = tuple.elem.size,
    outstanding = 0,
    pc = PC(0),
    rslt = Ob.NIV,
    trgt = Ob.NIV,
    argvec = tuple,
    env = ctxt.env,
    ctxt = ctxt,
    code = ctxt.code,
    self2 = ctxt.self2,
    selfEnv = ctxt.selfEnv,
    rcvr = ctxt.rcvr,
    monitor = ctxt.monitor,
    id = ctxt.id + 1000
  )

  def apply(trgt: Ob, argvec: Tuple): Ctxt = empty.copy(trgt = trgt, argvec = argvec)

  /** Save result to a location in the continuation of a given `ctxt`
    *
    * The location is defined by `tag`.
    * `applyK` potentially returns a `ctxt` which needs to be scheduled to the VM.
    *
    * The first element in the return tuple is false if storing was successful
    * and true if it failed.
    *
    * TODO: `applyK` does not need access to `GlobalEnv`
    */
  def applyK(result: Ob, tag: Location): CtxtTransition[Boolean] = {

    /**
      * This transformation is needed because `result` has to be received in
      * `ctxt.ctxt` (the continuation of the current `ctxt`) and not `ctxt` itself.
      */
    def transformToRcvInCont: CtxtTransition[Boolean] => CtxtTransition[Boolean] =
      trans =>
        trans.transformS[(GlobalEnv, Ctxt)](
          { case (globalEnv, ctxt) => (globalEnv, ctxt.ctxt) },
          (state, rcvGlobalEnvAndCont) => {
            val globalEnv = state._1
            val ctxt      = state._2
            val cont      = rcvGlobalEnvAndCont._2

            (globalEnv, ctxt.copy(ctxt = cont))
          }
      )

    transformToRcvInCont(rcv(result, tag))
  }

  /**
    * Try to save a value to a particular location in the given `ctxt`.
    * If successfully saved and `outstanding` equals zero, schedule the `ctxt`.
    *
    * The return value is false if storing was successful and true if it failed.
    *
    * TODO: `rcv` does not need access to `GlobalEnv`
    */
  def rcv(result: Ob, loc: Location): CtxtTransition[Boolean] = {
    def scheduleIfStoredAndOutstandingIsZero(storeResult: StoreResult,
                                             outstanding: Int,
                                             ctxt: Ctxt): CtxtTransition[Boolean] =
      storeResult match {
        case Success =>
          for (_ <- if (outstanding == 0) tellCont(List(ctxt)) else pureCtxt[Unit](())) yield false

        case Failure => pureCtxt[Boolean](true)
      }

    for {
      storeRes     <- transformCtxtToCtxtTrans(store(loc, result))
      _            <- modifyCtxt(_.update(_ >> 'outstanding)(_ - 1))
      outstanding  <- inspectCtxt[Int](_.outstanding)
      continuation <- getCtxt
      res          <- scheduleIfStoredAndOutstandingIsZero(storeRes, outstanding, continuation)
    } yield res
  }

  val transformCtxtToCtxtTrans: State[Ctxt, StoreResult] => CtxtTransition[StoreResult] = trans =>
    liftRWS[Eval, Unit, List[Continuation], (GlobalEnv, Ctxt), StoreResult](
      trans.transformS[(GlobalEnv, Ctxt)]({ case (_, ctxt) => ctxt },
                                          (oldGlobalEnvAndCtxt, newCtxt) => {
                                            val (oldGlobalEnv, _) = oldGlobalEnvAndCtxt
                                            (oldGlobalEnv, newCtxt)
                                          }))

  /** Return result to the continuation of the given `ctxt`
    *
    * The return value is false if storing was successful and true if it failed.
    *
    * `ret` potentially schedules the continuation of the given `ctxt`.
    * This happens when `outstanding` equals 1 and the last argument is about
    * to be received.
    * A scheduled continuation is captured in the writer monad of the
    * `CtxtTransition` type.
    *
    * The location we want to save the result to is given by the tag field.
    * If the tag field contains no location (`LocLimbo`), false is returned.
    * Otherwise the result of applyK is returned.
    *
    * TODO: `ret` does not need the `GlobalEnv`.
    */
  def ret(result: Ob): CtxtTransition[Boolean] =
    for {
      tag <- inspectCtxt[Location](_.tag)
      res <- tag match {
              case Limbo => pureCtxt[Boolean](false)
              case _     => applyK(result, tag)
            }
    } yield res

  def setReg(r: Int, ob: Ob): State[Ctxt, StoreResult] = {
    lazy val p = State.pure[Ctxt, StoreResult](Failure)

    def modify(f: Ctxt => Ctxt): State[Ctxt, StoreResult] =
      State.apply[Ctxt, StoreResult](f andThen ((_, Success)))

    r match {
      case 0 => modify(_.copy(rslt = ob))
      case 1 => modify(_.copy(trgt = ob))
      case 2 => modify(_.copy(argvec = ob.asInstanceOf[Tuple]))
      case 3 => modify(_.copy(env = ob))
      case 4 => modify(_.copy(code = ob.asInstanceOf[Code]))
      case 5 => modify(_.copy(ctxt = ob.asInstanceOf[Ctxt]))
      case 6 => modify(_.copy(self2 = ob))
      case 7 => modify(_.copy(selfEnv = ob))
      case 8 => modify(_.copy(rcvr = ob))
      case 9 => modify(_.copy(monitor = ob.asInstanceOf[Monitor]))
      case _ => p
    }
  }

  object NIV
      extends Ctxt(null, 0, 0, null, null, null, null, null, null, null, null, null, null, null)

  object PLACEHOLDER
      extends Ctxt(null, 0, 0, null, null, null, null, null, null, null, null, null, null, null)
}
