package coop.rchain.roscala.ob

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.{LocLimbo, LocRslt, Location}
import coop.rchain.roscala.Location._
import coop.rchain.roscala.Vm.State
import Ctxt.logger

case class Ctxt(var tag: Location,
                var nargs: Int,
                var outstanding: Int,
                var pc: Int,
                var rslt: Ob,
                var trgt: Ob,
                var argvec: Tuple,
                var env: Ob,
                var code: Code,
                var ctxt: Ctxt,
                var self2: Ob,
                var selfEnv: Ob,
                var rcvr: Ob,
                var monitor: Monitor)
    extends Ob {

  def applyK(result: Ob, loc: Location, state: State): Boolean =
    // Make continuation receive `result` at `tag`
    ctxt.rcv(result, loc, state)

  def arg(n: Int): Ob = argvec.value(n)

  def rcv(result: Ob, loc: Location, state: State): Boolean =
    if (store(loc, this, result)) {
      logger.debug("Store failure in Ctxt.rcv")
      true
    } else {
      outstanding -= 1
      if (outstanding == 0) {
        logger.debug("Scheduling continuation")
        scheduleStrand(state)
      } else {
        logger.debug(s"$outstanding outstanding argument in continuation")
      }
      false
    }

  def ret(result: Ob, state: State): Boolean = {
    logger.debug(s"Return $result to $tag in continuation")

    tag match {
      case LocLimbo => false
      case _ =>
        applyK(result, tag, state)
    }
  }

  def scheduleStrand(state: State): Unit =
    state.strandPool += this

  def setReg(reg: Int, ob: Ob): Unit =
    reg match {
      case 0 => this.rslt = ob
      case 1 => this.trgt = ob
      case 2 => this.argvec = ob.asInstanceOf[Tuple]
      case 3 => this.env = ob
      case 4 => this.code = ob.asInstanceOf[Code]
      case 5 => this.ctxt = ob.asInstanceOf[Ctxt]
      case 6 => this.self2 = ob
      case 7 => this.selfEnv = ob
      case 8 => this.rcvr = ob
      case 9 => this.monitor = ob.asInstanceOf[Monitor]
      case _ => throw new IllegalArgumentException("Unknown register")
    }
}

object Ctxt {
  val logger = Logger("Ctxt")

  def apply(code: Code, continuation: Ctxt, tag: Location): Ctxt =
    new Ctxt(
      tag = tag,
      nargs = 0,
      outstanding = 1,
      pc = 0,
      rslt = Niv,
      trgt = Niv,
      argvec = Tuple(null),
      env = Niv,
      code = code,
      ctxt = continuation,
      self2 = Niv,
      selfEnv = Niv,
      rcvr = Niv,
      monitor = null
    )

  def apply(tuple: Tuple, continuation: Ctxt): Ctxt =
    new Ctxt(
      tag = LocRslt,
      nargs = tuple.value.length,
      outstanding = 0,
      pc = 0,
      rslt = Niv,
      trgt = Niv,
      argvec = tuple,
      env = continuation.env,
      ctxt = continuation,
      code = continuation.code,
      self2 = continuation.self2,
      selfEnv = continuation.selfEnv,
      rcvr = continuation.rcvr,
      monitor = continuation.monitor
    )

  /**
    * Useful for testing.
    */
  def argvec(i: Int): Ctxt = new Ctxt(
    tag = LocLimbo,
    nargs = 0,
    outstanding = 0,
    pc = 0,
    rslt = Niv,
    trgt = Niv,
    argvec = Tuple(new Array[Ob](i)),
    env = Niv,
    code = Code(litvec = Seq.empty, codevec = Seq.empty),
    ctxt = null,
    self2 = Niv,
    selfEnv = Niv,
    rcvr = Niv,
    monitor = null
  )

  /**
    * Useful for testing.
    */
  def outstanding(i: Int): Ctxt = new Ctxt(
    tag = LocLimbo,
    nargs = 0,
    outstanding = i,
    pc = 0,
    rslt = Niv,
    trgt = Niv,
    argvec = Tuple(null),
    env = Niv,
    code = Code(litvec = Seq.empty, codevec = Seq.empty),
    ctxt = null,
    self2 = Niv,
    selfEnv = Niv,
    rcvr = Niv,
    monitor = null
  )
}
