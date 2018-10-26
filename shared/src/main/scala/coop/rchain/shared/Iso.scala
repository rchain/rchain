package coop.rchain.shared
import shapeless.LabelledGeneric

trait Iso[A, B] {
  def to(a: A): B
  def from(b: B): A
}

object Iso {
  implicit def simpleIso[A, B, Repr](
      implicit
      lGenA: LabelledGeneric.Aux[A, Repr],
      lGenB: LabelledGeneric.Aux[B, Repr]
  ) = new Iso[A, B] {
    override def to(a: A): B =
      lGenB.from(lGenA.to(a))
    override def from(b: B): A =
      lGenA.from(lGenB.to(b))
  }

  def apply[A, B](implicit iso: Iso[A, B]) = iso
}
