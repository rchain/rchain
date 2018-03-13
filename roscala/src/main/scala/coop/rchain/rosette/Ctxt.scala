package coop.rchain.rosette

import coop.rchain.rosette.VirtualMachine.loggerStrand
import Location._
import cats.data.State
import cats.data.State._
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
  type CtxtTransition[A] = State[Ctxt, A]

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

  /** Save result to a location in the parent ctxt.
    *
    * The location is defined by tag.
    * applyK potentially returns a ctxt which needs to be scheduled to the VM.
    */
  def applyK(result: Ob, tag: Location): CtxtTransition[(Boolean, Option[Continuation])] =
    rcv(result, tag)
      .transformS[Ctxt](_.ctxt, (child, updatedParent) => child.copy(ctxt = updatedParent))

  /**
    * Try to save a value to a particular location in the given ctxt.
    * If successfully saved and outstanding equals zero, schedule the ctxt.
    */
  def rcv(result: Ob, loc: Location): CtxtTransition[(Boolean, Option[Continuation])] =
    for {
      storeRes         <- store(loc, result)
      _                <- modify[Ctxt](_.update(_ >> 'outstanding)(_ - 1))
      outstanding      <- inspect[Ctxt, Int](_.outstanding)
      continuationCtxt <- get[Ctxt]
    } yield {
      storeRes match {
        case Success =>
          if (outstanding == 0)
            (false, Some(continuationCtxt))
          else
            (false, None)

        case Failure => (true, None)
      }
    }

  /** Return result to parent ctxt.
    *
    * This potentially returns the parent ctxt which then has to be scheduled
    * to the VM by the caller of ret.
    *
    * The location we want to save the result to is given by the tag field.
    * If the tag field contains no location (equals LocLimbo), (false, None) is returned.
    * Otherwise the result of applyK is returned.
    */
  def ret(result: Ob): CtxtTransition[(Boolean, Option[Continuation])] =
    for {
      tag <- inspect[Ctxt, Location](_.tag)
      res <- tag match {
              case Limbo => pure[Ctxt, (Boolean, Option[Continuation])](false, None)
              case _     => applyK(result, tag)
            }
    } yield res

  def setReg(r: Int, ob: Ob): CtxtTransition[StoreResult] = {
    lazy val p = pure[Ctxt, StoreResult](Failure)

    def modify(f: Ctxt => Ctxt): CtxtTransition[StoreResult] =
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
