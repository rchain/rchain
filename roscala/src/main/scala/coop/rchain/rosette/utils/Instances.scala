package coop.rchain.rosette.utils

import coop.rchain.rosette.{Actor, Ctxt, Ob, StdExtension}
import shapeless._

import scala.annotation.tailrec

object Instances {
  case object InvalidLensParam extends Exception

  private def setField[T: Clone, A](arg: T, fieldName: String)(value: A): T = {
    val r = implicitly[Clone[T]].clone(arg)

    @tailrec
    def rec(clazz: Class[_]): T =
      try {
        val field = clazz.getDeclaredField(fieldName)
        field.setAccessible(true)
        field.set(r, value)

        r
      } catch {
        case _: NoSuchFieldException => rec(clazz.getSuperclass)
      }

    rec(arg.getClass)
  }

  implicit def cloneOb[T <: Ob]: Clone[T] = _.clone().asInstanceOf[T]

  //instance to create lens for Ob over the `slot` field, like:
  //lens[Ob] >> 'slot
  implicit val mkSlotFieldLens = new MkFieldLens[Ob, Witness.`'slot`.T] {
    override type Elem = Seq[Ob]
    override def apply(): Lens[Ob, Seq[Ob]] = new Lens[Ob, Seq[Ob]] {
      override def get(s: Ob): Seq[Ob]        = s.slot
      override def set(s: Ob)(a: Seq[Ob]): Ob = setField(s, "slot")(a)
    }
  }

  //instance to create lens for Ob over the `parent` field, like:
  //lens[Ob] >> 'parent
  implicit val mkParentFieldLens = new MkFieldLens[Ob, Witness.`'parent`.T] {
    override type Elem = Ob
    override def apply(): Lens[Ob, Ob] = new Lens[Ob, Ob] {
      override def get(s: Ob): Ob =
        if (s == null) throw InvalidLensParam
        else s.parent
      override def set(s: Ob)(a: Ob): Ob =
        setField(s, "parent")(a)
    }
  }

  //instance to create lens for Actor over the `extension` field, like:
  //lens[Actor] >> 'field
  implicit val mkExtensionFieldLens =
    new MkFieldLens[Actor, Witness.`'extension`.T] {
      override type Elem = StdExtension
      override def apply(): Lens[Actor, StdExtension] =
        new Lens[Actor, StdExtension] {
          override def get(s: Actor): StdExtension = s.extension
          override def set(s: Actor)(a: StdExtension): Actor =
            setField(s, "extension")(a)
        }
    }

  //instance to create lens for Ctxt over the `selfEnv` field, like:
  //lens[Ctxt] >> 'selfEnv
  implicit val mkSelfEnvFieldLens =
    new MkFieldLens[Ctxt, Witness.`'selfEnv`.T] {
      override type Elem = Ob
      override def apply(): Lens[Ctxt, Ob] =
        new Lens[Ctxt, Ob] {
          override def get(s: Ctxt): Ob = s.selfEnv
          override def set(s: Ctxt)(a: Ob): Ctxt =
            setField(s, "selfEnv")(a)
        }
    }

  //instance to create lens for Ob over the `meta` field, like:
  //lens[Ob] >> 'meta
  implicit val mkMetaFieldLens = new MkFieldLens[Ob, Witness.`'meta`.T] {
    override type Elem = Ob
    override def apply(): Lens[Ob, Ob] = new Lens[Ob, Ob] {
      override def get(s: Ob): Ob =
        if (s == null) throw InvalidLensParam
        else s.meta
      override def set(s: Ob)(a: Ob): Ob =
        setField(s, "meta")(a)
    }
  }
}
