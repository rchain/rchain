package coop.rchain.rosette

import java.io.File

import coop.rchain.rosette.utils.printToFile
import coop.rchain.rosette.Meta.StdMeta
import coop.rchain.rosette.Ob.{ObTag, SysCode}
import coop.rchain.rosette.prim.Prim
import shapeless.OpticDefns.RootLens
import shapeless._

trait Base

//TODO change type of `indirect` argument to bool
trait Ob extends Base {
  val slot: Seq[Ob] = Nil

  val obTag: ObTag = null
  val sysval: SysCode = null
  val constantP = true

  def meta: Ob = slot.head
  def parent: Ob = slot(1)

  def dispatch(state: VMState): (Result, VMState) = null
  def extendWith(keyMeta: Ob): Ob = null

  def getAddr(ind: Int, level: Int, offset: Int): Ob =
    getLex(ind, level, offset)

  def getField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               sign: Int): Ob =
    ??? //TODO

  def getLex(ind: Int, level: Int, offset: Int): Ob = {
    val p: Ob = nthParent(level)

    actorExtension(ind, p)
      .map(offsetOrInvalid(offset, _))
      .getOrElse(Ob.INVALID)
  }

  def is(value: Ob.ObTag): Boolean = true

  def lookup(key: Ob, ctxt: Ctxt): Result =
    Right(null)

  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Result =
    Right(null)

  def lookupAndInvoke(state: VMState): (Result, VMState) = {
    val fn = meta match {
      case stdMeta: StdMeta =>
        stdMeta.lookupOBOStdMeta(self, state.ctxt.trgt)(state)
      case _ => Left(Absent)
    }

    if (state.interruptPending != 0) {
      (Left(Absent), state)
    } else {
      // TODO:
      //if (fn == ABSENT) {
      //  PROTECT_THIS(Ob); PROTECT(ctxt);
      //  ctxt->prepare();
      //  Tuple* new_argvec = Tuple::create (2, INVALID);
      //  new_argvec->elem(0) = ctxt->trgt;
      //  new_argvec->elem(1) = ctxt;
      //  Ctxt* new_ctxt = Ctxt::create (oprnMissingMethod, new_argvec);
      //  new_ctxt->monitor = vm->systemMonitor;
      //  return oprnMissingMethod->dispatch(new_ctxt);
      //}

      fn match {
        case Right(prim: Prim) => prim.invoke(state)
        case _ => (Left(Absent), state)
      }
    }
  }

  def matches(ctxt: Ctxt): Boolean = false
  def numberOfSlots(): Int = slot.size
  def runtimeError(msg: String, state: VMState): (RblError, VMState) =
    (DeadThread, state)

  def setAddr(ind: Int, level: Int, offset: Int, value: Ob): Ob =
    setLex(ind, level, offset, value)

  def setField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               value: Int): Ob =
    ??? //TODO

  def setLex(ind: Int, level: Int, offset: Int, value: Ob): Ob = Ob.INVALID

  def notImplemented(opName: String): Unit = {
    val className = this.getClass.getSimpleName
    System.err.println(s"$className#$opName is not implemented!")
  }

  def notImplemented(): Unit = {
    val callingMethodName =
      Thread.currentThread.getStackTrace()(2).getMethodName
    notImplemented(callingMethodName)
  }

  def notImplementedOb(): Ob = {
    val callingMethodName =
      Thread.currentThread.getStackTrace()(2).getMethodName
    notImplemented(callingMethodName)
    Ob.INVALID
  }

  def forwardingAddress: Ob = meta

  def self: Ob = this

  def inlineablePrimP: Either[RblError, Prim] = Left(Invalid)

  def emptyMbox: Ob = Ob.INVALID

  def container: Ob = this

  def mailbox: Ob = emptyMbox

  def setMailbox(ob: Ob): Ob = self

  def addSlot(l: Ob, r: Ob): Int = {
    notImplemented()
    0
  }

  def dup: Ob = this

  def indexedSize: Ob = notImplementedOb()

  def setNth(i: Int, v: Ob): Option[Ob] = Some(notImplementedOb())
  def subObject(i1: Int, i2: Int): Ob = notImplementedOb()
  def asPathname: String = ""
  def nth(i: Int): Option[Ob] = Some(notImplementedOb())
  def slotNum() = 0 //TODO missing implementation

  private def actorExtension(ind: Int, p: Ob): Option[Ob] =
    if (ind > 0) {
      p match {
        case a: Actor =>
          Some(a.extension)
        case _ => None
      }
    } else {
      Some(p)
    }

  private def offsetOrInvalid(offset: Int, ob: Ob): Ob =
    if (offset < ob.numberOfSlots) {
      ob.slot(offset)
    } else {
      Ob.INVALID
    }

  private def nthParent(level: Int): Ob = recMap(level, this)(_.parent)
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

  object ABSENT extends Ob
  object INVALID extends Ob
  object NIV extends Ob
  object RBLTRUE extends Ob
  object RBLFALSE extends Ob
  object NilMeta extends Ob

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

  //TODO use logging framework
  def printOn[A <: Ob: Show](ob: A, file: File): Unit = {
    val str = Show[A].show(ob)
    printToFile(file)(_.print(str))
  }

  def printQuotedOn[A <: Ob: Show](ob: A, file: File): Unit =
    printOn(ob, file)

  def displayOn[A <: Ob: Show](ob: A, file: File): Unit =
    printOn(ob, file)
}
