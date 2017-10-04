package coop.rchain.rosette

import coop.rchain.rosette.Ob.ObTag
import shapeless._
import shapeless.OpticDefns.RootLens

sealed trait LookupError
case object Absent extends LookupError
case object Upcall extends LookupError

trait Base

trait Ob extends Base {
  val parent: Ob
  val meta: Ob
  val slot: Seq[Ob]
  val obTag: ObTag = null

  def extendWith(keymeta: Ob): Ob = null
  def extendWith(keymeta: Ob, argvec: Tuple): Ob = null
  def getAddr(ind: Int, level: Int, offset: Int): Ob = null
  def getField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               sign: Int): Ob =
    null
  def getLex(ind: Int, level: Int, offset: Int): Ob = null
  def is(value: Ob.ObTag): Boolean = true
  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Either[LookupError, Ob] =
    Right(null)
  def matches(ctxt: Ctxt): Boolean = false
  def numberOfSlots(): Int = Math.max(0, slot.length - 2)
  def setAddr(ind: Int, level: Int, offset: Int, value: Ob): Ob = null
  def setField(ind: Int,
               level: Int,
               offset: Int,
               spanSize: Int,
               value: Int): Ob = null
  def setLex(ind: Int, level: Int, offset: Int, value: Ob): Ob = null
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
}
