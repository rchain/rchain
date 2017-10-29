package coop.rchain.rosette

import java.io.{File, PrintWriter}

import coop.rchain.rosette.Ob.{ObTag, SysCode}
import shapeless._
import shapeless.OpticDefns.RootLens

sealed trait LookupError
case object Absent extends LookupError
case object Upcall extends LookupError

trait Base

//TODO change type of `indirect` argument to bool
trait Ob extends Base {
  val meta: Ob
  val parent: Ob
  val slot: Seq[Ob]
  val obTag: ObTag = null
  val sysval: SysCode = null
  val constantP = true

  def dispatch(ctxt: Ctxt): Ob = null
  def extendWith(keymeta: Ob): Ob = null
  def extendWith(keymeta: Ob, argvec: Tuple): Ob = null

  def getAddr(ind: Int, level: Int, offset: Int): Ob = getLex(ind, level, offset) //TODO FIXNUM(ADDR_TO_PRE_FIXNUM _)

  def getField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               sign: Int): Ob = {

    ??? //TODO
  }

  def getLex(ind: Int, level: Int, offset: Int): Ob = {
    val p: Ob = nthParent(level)

    actorExtensionFiltered(ind, p)
      .map(offsetOrInvalid(offset, _))
      .getOrElse(Ob.INVALID)
  }

  def is(value: Ob.ObTag): Boolean = true
  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Either[LookupError, Ob] =
    Right(null)
  def matches(ctxt: Ctxt): Boolean = false
  def numberOfSlots(): Int = Math.max(0, slot.length - 2)
  def runtimeError(msg: String, state: VMState): (RblError, VMState) =
    (DeadThread, state)

  def setAddr(ind: Int, level: Int, offset: Int, value: Ob): Ob = setLex(ind, level, offset, value) //TODO

  def setField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               value: Int): Ob = {
    ??? //TODO
  }

  def setLex(ind: Int, level: Int, offset: Int, value: Ob): Ob = {
    val p: Ob = nthParent(level)

    actorExtensionFiltered(ind, p)
      .filter { _ => offset < p.numberOfSlots() }
      .map { ob =>
        ob.slot(offset) = value
        value
      } getOrElse Ob.INVALID
  }

  def notImplemented(opName: String): Unit = {
    val className = this.getClass.getSimpleName
    System.err.println(s"$className#$opName is not implemented!")
  }

  def notImplemented(): Unit = {
    val callingMethodName = Thread.currentThread.getStackTrace()(2).getMethodName
    notImplemented(callingMethodName)
  }

  def notImplementedOb(): Ob = {
    val callingMethodName = Thread.currentThread.getStackTrace()(2).getMethodName
    notImplemented(callingMethodName)
    Ob.INVALID
  }

  def forwardingAddress: Ob = meta

  def self: Ob = this

  def inlineablePrimP: Prim = Prim.INVALID

  def emptyMbox: Ob = Ob.INVALID

  def container: Ob = this

  def mailbox: Ob = emptyMbox

  def setMailbox: Ob = self

  def rcons(ob: Ob): Ob =
    copy(slot = slot :+ ob)

  def copy(parent: Ob = parent, meta: Ob = meta, slot: Seq[Ob] = slot): Ob = {
    val values = (parent, meta, slot)
    new Ob {
      override val parent: Ob = values._1
      override val meta: Ob = values._2
      override val slot: Seq[Ob] = values._3
    }
  }

  def addSlot(l: Ob, r: Ob): Int = {
    notImplemented()
    0
  }

  def dup: Ob = this

  def indexedSize: Ob = notImplementedOb()

  def setNth(i: Int, v: Ob): Ob = notImplementedOb()
  def subObject(i1: Int, i2: Int): Ob = notImplementedOb()
  def asPathname: String = ""
  def nth(i: Int): Ob = notImplementedOb()

  private def actorExtensionFiltered(ind: Int, p: Ob): Option[Ob] = {
    if (p.numberOfSlots() <= SLOT_NUM(Actor, extension)) { //TODO
      None
    } else {
      Some(actorExtension(ind, p))
    }
  }

  private def actorExtension(ind: Int, p: Ob): Ob = {
    if (ind > 0) {
      p match {
        case a: Actor =>
          a.extension
      }
    } else p
  }

  private def offsetOrInvalid(offset: Int, ob: Ob): Ob =
    if (offset < ob.numberOfSlots())
      ob.slot(offset)
    else
      Ob.INVALID

  private def base(ob: Ob): Ob = ???

  private def nthParent(level: Int): Ob = base(recMap(level, this)(_.parent))
}

object Ob {
  sealed trait ObTag
  case object OTptr extends ObTag
  case object OTsym extends ObTag
  case object OTfixnum extends ObTag
  case object OTesc extends ObTag
  case object OTbool extends ObTag
  case object OTchar extends ObTag
  case object OTniv extends ObTag
  case object OTsysval extends ObTag
  case object OTlocation extends ObTag

  sealed trait SysCode
  case object SyscodeInvalid extends SysCode
  case object SyscodeUpcall extends SysCode
  case object SyscodeSuspend extends SysCode
  case object SyscodeInterrupt extends SysCode
  case object SyscodeSleep extends SysCode
  case object SyscodeDeadThread extends SysCode

  case object ABSENT extends Ob {
    override val parent = null
    override val meta = null
    override val slot = null
  }

  case object INVALID extends Ob {
    override val parent = null
    override val meta = null
    override val slot = null
  }

  case object NIV extends Ob {
    override val parent = null
    override val meta = null
    override val slot = null
  }

  object RBLTRUE extends Ob {
    override val parent = null
    override val meta = null
    override val slot = null
  }

  object RBLFALSE extends Ob {
    override val parent = null
    override val meta = null
    override val slot = null
  }

  object Lenses {
    def setA[T, A](a: A)(f: RootLens[A] ⇒ Lens[A, T])(value: T): A =
      f(lens[A]).set(a)(value)

    def updateA[T, A](a: A)(f: RootLens[A] ⇒ Lens[A, T])(value: T => T): A =
      f(lens[A]).modify(a)(value)

    implicit class LensBase[A <: Base](val base: A) extends AnyVal {
      def set[T](f: RootLens[A] ⇒ Lens[A, T])(value: T): A =
        setA(base)(f)(value)

      def update[T](f: RootLens[A] ⇒ Lens[A, T])(value: T => T): A =
        updateA(base)(f)(value)

      def updateSelf[T](value: A => A): A = value(base)
    }
  }

  def printOn[A <: Ob: Show](ob: A, file: File): Unit = {
    val str = Show[A].show(ob)
    printToFile(file)(_.print(str))
  }

  def printQuotedOn[A <: Ob: Show](ob: A, file: File): Unit =
    printOn(ob, file)

  def displayOn[A <: Ob: Show](ob: A, file: File): Unit =
    printOn(ob, file)

  //TODO move to another class
  private def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
