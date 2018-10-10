package coop.rchain.roscala.ob

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.typesafe.scalalogging.Logger
import coop.rchain.roscala.{LocLimbo, LocRslt, Location}
import coop.rchain.roscala.Location._
import coop.rchain.roscala.Vm.State
import Ctxt.logger

//TODO make access to various fields thread safe
class Ctxt(
    var tag: Location,
    var nargs: Int,
    val outstanding: AtomicInteger = new AtomicInteger(0),
    var pc: Int,
    var argvec: Tuple,
    var env: Ob,
    var code: Code,
    var ctxt: Option[Ctxt],
    var self2: Ob,
    var selfEnv: Ob,
    var rcvr: Ob,
    var monitor: Monitor
) extends Ob {

  private val _rslt = new AtomicReference[Ob](Niv)
  private val _trgt = new AtomicReference[Ob](Niv)

  /*
    Needed because these fields are modified concurrently
   */
  @inline def rslt: Ob            = _rslt.get()
  @inline def rslt_=(v: Ob): Unit = _rslt.set(v)

  @inline def trgt: Ob            = _trgt.get()
  @inline def trgt_=(v: Ob): Unit = _trgt.set(v)

  def applyK(result: Ob, loc: Location, state: State): Boolean =
    // Make continuation receive `result` at `tag`
    ctxt.get.rcv(result, loc, state)

  def arg(n: Int): Ob = this.argvec.value(n)

  override def clone(): Ctxt = {
    val res = new Ctxt(
      tag = this.tag,
      nargs = this.nargs,
      outstanding = new AtomicInteger(this.outstanding.get()),
      pc = this.pc,
      argvec = Tuple(this.argvec.value.clone()),
      env = this.env,
      code = this.code,
      ctxt = this.ctxt,
      self2 = this.self2,
      selfEnv = this.selfEnv,
      rcvr = this.rcvr,
      monitor = this.monitor
    )

    res.rslt = this.rslt
    res.trgt = this.trgt

    res
  }

  def rcv(result: Ob, loc: Location, state: State): Boolean =
    if (store(loc, this, result)) {
      logger.debug("Store failure in Ctxt.rcv")
      true
    } else {
      if (outstanding.decrementAndGet() == 0) {
        logger.debug(s"Scheduling continuation $this")
        scheduleStrand(state)
      } else {
        logger.debug(s"$outstanding outstanding argument in continuation ($this)")
      }
      false
    }

  def ret(result: Ob, state: State): Boolean = {
    logger.debug(s"Write $result to $tag in continuation (${this.ctxt.get})")

    tag match {
      case LocLimbo => false
      case _ =>
        applyK(result, tag, state)
    }
  }

  def scheduleStrand(state: State): Unit =
    state.strandPool.append((this, state))

  def reg(reg: Int): Ob =
    reg match {
      case 0 => this.rslt
      case 1 => this.trgt
      case 2 => this.argvec
      case 3 => this.env
      case 4 => this.code
      case 5 => this.ctxt.getOrElse(Niv)
      case 6 => this.self2
      case 7 => this.selfEnv
      case 8 => this.rcvr
      case 9 => this.monitor
      case _ => throw new IllegalArgumentException("Unknown register")
    }

  def setReg(reg: Int, ob: Ob): Unit =
    reg match {
      case 0 => this.rslt = ob
      case 1 => this.trgt = ob
      case 2 => this.argvec = ob.asInstanceOf[Tuple]
      case 3 => this.env = ob
      case 4 => this.code = ob.asInstanceOf[Code]
      case 5 => this.ctxt = Some(ob.asInstanceOf[Ctxt])
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
      outstanding = new AtomicInteger(1),
      pc = 0,
      argvec = Tuple(new Array[Ob](0)),
      env = Niv,
      code = code,
      ctxt = Some(continuation),
      self2 = Niv,
      selfEnv = Niv,
      rcvr = Niv,
      monitor = null
    )

  def apply(tuple: Tuple, continuation: Ctxt): Ctxt =
    new Ctxt(
      tag = LocRslt,
      nargs = tuple.value.length,
      pc = 0,
      argvec = tuple,
      env = continuation.env,
      ctxt = Some(continuation),
      code = continuation.code,
      self2 = continuation.self2,
      selfEnv = continuation.selfEnv,
      rcvr = continuation.rcvr,
      monitor = continuation.monitor
    )

  def apply(continuation: Ctxt): Ctxt =
    new Ctxt(
      tag = LocRslt,
      nargs = 0,
      pc = 0,
      argvec = Nil,
      env = continuation.env,
      ctxt = Some(continuation),
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
    pc = 0,
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
  def empty: Ctxt = new Ctxt(
    tag = LocLimbo,
    nargs = 0,
    pc = 0,
    argvec = Tuple(new Array[Ob](0)),
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
    outstanding = new AtomicInteger(i),
    pc = 0,
    argvec = Tuple(new Array[Ob](0)),
    env = Niv,
    code = Code(litvec = Seq.empty, codevec = Seq.empty),
    ctxt = null,
    self2 = Niv,
    selfEnv = Niv,
    rcvr = Niv,
    monitor = null
  )
}
