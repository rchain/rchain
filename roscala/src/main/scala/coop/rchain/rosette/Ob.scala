package coop.rchain.rosette

import java.io.File

import cats.data.State
import cats.data.State._
import coop.rchain.rosette
import coop.rchain.rosette.utils.{lensTrans, printToFile, unsafeCastLens}
import coop.rchain.rosette.Meta.StdMeta
import coop.rchain.rosette.Ob.{ObTag, SysCode}
import coop.rchain.rosette.prim.Prim
import coop.rchain.rosette.utils.Instances._
import shapeless.OpticDefns.RootLens
import shapeless._
import cats.implicits._
import coop.rchain.rosette.Ctxt.{Continuation, CtxtTransition}

trait Base

trait Ob extends Base with Cloneable {
  val slot: Seq[Ob] = Nil

  val obTag: ObTag    = null
  val sysval: SysCode = null
  val constantP       = true

  def meta: Ob   = slot.head
  def parent: Ob = slot(1)

  def dispatch: CtxtTransition[(Result, Option[Continuation])] = pure((Right(Ob.NIV), None))
  def extendWith(keyMeta: Ob): Ob                              = null

  def getAddr(indirect: Boolean, level: Int, offset: Int): Ob =
    getLex(indirect, level, offset)

  def getField(indirect: Boolean, level: Int, offset: Int, spanSize: Int, sign: Int): Ob =
    ??? //TODO

  def getLex(indirect: Boolean, level: Int, offset: Int): Ob = {
    val p: Ob = (0 until level).foldLeft(this)((ob, _) => ob.parent)

    actorExtension(indirect, p)
      .map(offsetOrInvalid(offset, _))
      .getOrElse(Ob.INVALID)
  }

  def is(value: Ob.ObTag): Boolean = false

  def lookup(key: Ob, ctxt: Ctxt): Result =
    Right(null)

  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Result =
    Right(null)

  def lookupAndInvoke: CtxtTransition[(Result, Option[Continuation])] =
    for {
      target <- State.inspect[Ctxt, Ob](_.trgt)
      fn     <- meta.asInstanceOf[StdMeta].lookupOBOStdMeta(self, target)
      result <- fn match {
                 case Right(prim: Prim) => prim.invoke
                 case _                 => pure[Ctxt, (Result, Option[Continuation])]((Left(Absent), None))
               }
    } yield result

  def matches(ctxt: Ctxt): Boolean = false
  def numberOfSlots(): Int         = slot.size
  def runtimeError(msg: String, state: VMState): (RblError, VMState) =
    (DeadThread, state)

  def setAddr(indirect: Boolean, level: Int, offset: Int, value: Ob): State[Ob, StoreResult] = ???

  def setField(indirect: Boolean, level: Int, offset: Int, spanSize: Int, value: Int): Ob =
    ??? //TODO

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
  def subObject(i1: Int, i2: Int): Ob   = notImplementedOb()
  def asPathname: String                = ""
  def nth(i: Int): Option[Ob]           = Some(notImplementedOb())
  def slotNum()                         = 0 //TODO missing implementation

  override def clone(): AnyRef = super.clone()

  private def actorExtension(indirect: Boolean, p: Ob): Option[Ob] =
    if (indirect) {
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
}

object Ob {
  sealed trait ObTag
  case object OTptr      extends ObTag
  case object OTsym      extends ObTag
  case object OTfixnum   extends ObTag
  case object OTesc      extends ObTag
  case object OTbool     extends ObTag
  case object OTchar     extends ObTag
  case object OTniv      extends ObTag
  case object OTsysval   extends ObTag
  case object OTlocation extends ObTag

  sealed trait SysCode
  case object SyscodeInvalid    extends SysCode
  case object SyscodeUpcall     extends SysCode
  case object SyscodeSuspend    extends SysCode
  case object SyscodeInterrupt  extends SysCode
  case object SyscodeSleep      extends SysCode
  case object SyscodeDeadThread extends SysCode

  object ABSENT   extends Ob
  object INVALID  extends Ob
  object NIV      extends Ob
  object RBLTRUE  extends Ob
  object RBLFALSE extends Ob
  object NilMeta  extends Ob

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

  def getLex(indirect: Boolean, level: Int, offset: Int): State[Ob, Option[Ob]] = ???

  def getAddr(indirect: Boolean, level: Int, offset: Int): State[Ob, Option[Ob]] = ???

  def setLex(indirect: Boolean, level: Int, offset: Int, value: Ob): State[Ob, StoreResult] =
    State { ob =>
      val nthParentLens =
        (0 until level).foldLeft(lens[Ob]: Lens[Ob, Ob])((l, _) => l >> 'parent)

      def inSlotNum(lens: Lens[Ob, Ob]): Option[Lens[Ob, Ob]] =
        if (!indirect) Some(lens)
        else {
          val numberOfSlots = lens.get(ob).numberOfSlots()
          if (ob.slotNum() >= numberOfSlots) None
          else {
            val resLens = unsafeCastLens[Actor](lens) >> 'extension
            Some(unsafeCastLens[Ob](resLens))
          }
        }

      def updateSlot(lens: Lens[Ob, Ob]): Option[Ob] =
        if (offset >= ob.numberOfSlots) None
        else {
          val slotLens = lens >> 'slot
          val res      = lensTrans(slotLens, ob)(_.updated(offset, value))
          Some(res)
        }

      try {
        inSlotNum(nthParentLens).flatMap(updateSlot) match {
          case Some(newOb) => (newOb, Success)
          case None        => (ob, Failure)
        }
      } catch {
        // TODO: Add logging
        case _: IndexOutOfBoundsException => (ob, Failure)
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
