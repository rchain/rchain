package coop.rchain.rosette

import java.io.File

import coop.rchain.rosette.Ob.{OTlocation, ObTag, SysCode}
import coop.rchain.rosette.utils.{pSlice, printToFile}
import coop.rchain.rosette.utils.Instances.eqOb
import shapeless.OpticDefns.RootLens
import shapeless._
import cats.implicits._

import scala.collection.mutable

sealed trait LookupError
case object Absent extends LookupError
case object Upcall extends LookupError

trait Base

//TODO change type of `indirect` argument to bool
trait Ob extends Base {
  val _slot: mutable.Seq[Ob]

  val obTag: ObTag = null
  val sysval: SysCode = null
  val constantP = true

  def meta: Ob = _slot.head
  def parent: Ob = _slot(1)
  lazy val slot: mutable.Seq[Ob] = pSlice(_slot, 2, _slot.size)

  def dispatch(ctxt: Ctxt): Ob = {
    //PROTECT(ctxt)

    val av = Tuple(2, Some(Ob.NIV))
    av.elem(0) = ctxt.trgt
    av.elem(1) = ctxt

    val c = Ctxt(???, av).copy(nargs = 2)
    c.trgt.dispatch(c)
  }

  def extendWith(keymeta: Ob): Ob = null
  def extendWith(keymeta: Ob, argvec: Tuple): Ob = null

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
  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Either[LookupError, Ob] =
    Right(null)
  def matches(ctxt: Ctxt): Boolean = false
  def numberOfSlots(): Int = slot.length
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

  def setLex(ind: Int, level: Int, offset: Int, value: Ob): Ob = {
    val p: Ob = nthParent(level)

    actorExtension(ind, p)
      .filter(_ => offset < p.numberOfSlots)
      .map { ob =>
        //TODO remove side effect here
        ob.slot(offset) = value
        value
      } getOrElse Ob.INVALID
  }

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

  def inlineablePrimP: Prim = Prim.INVALID

  def emptyMbox: Ob = Ob.INVALID

  def container: Ob = this

  def mailbox: Ob = emptyMbox

  def setMailbox(ob: Ob): Ob = self

  def rcons(ob: Ob): Ob =
    copyOb(slot = slot :+ ob)

  def copyOb(parent: Ob = parent,
             meta: Ob = meta,
             slot: mutable.Seq[Ob] = slot): Ob = {
    val (p, m, s) = (parent, meta, slot)
    new Ob {
      override val _slot: mutable.Seq[Ob] = m +: p +: s
    }
  }

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

  def get(ob1: Ob, ob2: Ob, ctxt: Ctxt): Ob = notImplementedOb()

  def setOb(ob1: Ob, ob2: Ob, ob3: Ob, ctxt: Ctxt): Ob = notImplementedOb()

  def isSynchronousTrgt: Boolean = true

  def updateNoArgs: Ob = self

  def invoke(ctxt: Ctxt): Ob = {
    if (ctxt.nargs === 1) {
      val me = self
      ctxt.ret(me)

      if (!ctxt.trgt.isSynchronousTrgt) {
        ctxt.arg(0) foreach (_.updateNoArgs)
      }

      me
    } else {
      ctxt.trgt.runtimeError("Bad method", ctxt)
    }
  }

  def lookup(key: Ob, ctxt: Ctxt): Either[RblError,Ob] =
    if (interruptPending) {
      Ob.ABSENT
    } else {
      val me = self

      val result = meta.get(me, key, ctxt)

      if (result === Ob.ABSENT) {
        parent.lookup(key, ctxt)
      } else {
        Right(result)
      }
    }

  def lookupAndInvoke(state: VMState)(ctxt: Ctxt): Either[LookupError, Ob] = {
    val fn = meta.lookupOBO(self, ctxt.trgt, ctxt)

    fn map { f =>
      if (interruptPending) {
        Ob.ABSENT
      } else if (f === Ob.ABSENT) {

        val prepared = ctxt.prepare()
        val newArgvec = Tuple(2, Some(Ob.INVALID))
        newArgvec.elem(0) = prepared.trgt
        newArgvec.elem(1) = prepared

        val newCtxt = Ctxt(Prim.oprnMissingMethod, newArgvec)
          .copy(monitor = state.systemMonitor)

        Prim.oprnMissingMethod.dispatch(newCtxt)
      } else {
        f.invoke(ctxt)
      }
    }
  }

  def nextMsg(mboxOb: MboxOb, ob: Ob): Ob = notImplementedOb()

  def updateOb(enabledSetProvided: Boolean, ctxt: Ctxt): Ob = {
    val keyStart = if (enabledSetProvided) 1 else 0

    val me = self
    var rslt = me

    if (ctxt.nargs > keyStart) {
      for {
        i <- keyStart.until(ctxt.nargs-1, 2)
        if rslt === me
        arg1 <- ctxt.arg(i)
        arg2 <- ctxt.arg(i + 1)
      } {
        rslt = meta.setOb(me, arg1, arg2, ctxt)
      }
    }

    rslt
  }

  def updateByLoc(enabledSetProvided: Boolean, ctxt: Ctxt): Ob = {
    /*
        for (int i = key_start; i < ctxt->nargs-1; i += 2) {
        if (!IS(OTlocation, ctxt->arg(i)))
      return actorUpdateBang->runtimeError(ctxt, "bad location descriptor");
        Location loc;
        loc.atom = ctxt->arg(i);
        setValWrt(loc, this, ctxt->arg(i+1));
    }
     */
    val keyStart = if (enabledSetProvided) 1 else 0

    if (ctxt.nargs > keyStart) {
      for {
        i <- keyStart.until(ctxt.nargs, 2)
        arg <- ctxt.arg(i)
      } {
        val atom = LocationAtom(arg)
        //Location.setValWrt(atom, this, )
        ???
      }
    }
    ???
  }

  def primitiveInitialize(ctxt: Ctxt): Ob = {
    val n = math.min(ctxt.nargs - 1, numberOfSlots())

    for {
      i <- 0 until n
      arg <- ctxt.arg(i+1)
    } {
      self.slot(i) = arg
    }

    self
  }

  def receive(ctxt: Ctxt): Either[LookupError,Option[Ob]] = {
    ctxt.arg(0).map(_.lookupAndInvoke(ctxt)).sequenceU
  }

  def receiveMsg(mboxOb: MboxOb, ctxt: Ctxt): Ob = notImplementedOb()

  def becomeNew(state: VMState)(ob: Ob, ctxt: Ctxt): Either[RblError,Ob] = {
    runtimeError("Cannot become a new object", state) match {
      case (rblError,_) =>
        Left(rblError)
    }
  }

  def accepts(ctxt: Ctxt): Boolean = false

  def matches(ctxt: Ctxt): Boolean = false

  def isNullP: Ob = Ob.RBLFALSE

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

  case object ABSENT extends Ob {
    override val _slot: mutable.Seq[Ob] = null
  }

  case object INVALID extends Ob {
    override val _slot: mutable.Seq[Ob] = null
  }

  case object NIV extends Ob {
    override val _slot: mutable.Seq[Ob] = null
  }

  object RBLTRUE extends Ob {
    override val _slot: mutable.Seq[Ob] = null
  }

  object RBLFALSE extends Ob {
    override val _slot: mutable.Seq[Ob] = null
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
