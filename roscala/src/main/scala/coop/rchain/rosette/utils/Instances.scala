package coop.rchain.rosette.utils

import coop.rchain.rosette.{Actor, Ob, StdExtension}
import shapeless._

object Instances {

  private def setField[T: Clone, A](arg: T, fieldName: String)(value: A): T = {
    val r = implicitly[Clone[T]].clone(arg)

    val field = r.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.set(r, value)

    r
  }

  implicit def cloneOb[T <: Ob]: Clone[T] = _.clone().asInstanceOf[T]

  //instance to create lens for Ob over the `slot` field, like:
  //lens[Ob] >> 'slot
  implicit val mkSlotFieldLens = new MkFieldLens[Ob, Witness.`'slot`.T] {
    override type Elem = Seq[Ob]
    override def apply(): Lens[Ob, Seq[Ob]] = new Lens[Ob, Seq[Ob]] {
      override def get(s: Ob): Seq[Ob] = s.slot
      override def set(s: Ob)(a: Seq[Ob]): Ob = setField(s, "slot")(a)
    }
  }

  //instance to create lens for Ob over the `parent` field, like:
  //lens[Ob] >> 'parent
  implicit val mkParentFieldLens = new MkFieldLens[Ob, Witness.`'parent`.T] {
    override type Elem = Ob
    override def apply(): Lens[Ob, Ob] = new Lens[Ob, Ob] {
      override def get(s: Ob): Ob = s.parent
      override def set(s: Ob)(a: Ob): Ob = {
        val l = lens[Ob] >> 'slot
        val r: Seq[Ob] = l.get(s).updated(1, a)
        l.set(s)(r)
      }
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
}
