package coop.rchain.models

import scalapb.TypeMapper

class AlwaysEqual[A](val item: A) {
  def get(): A = item

  override def equals(other: Any): Boolean = other match {
    case _: (AlwaysEqual[A]) => true
    case _                   => false
  }

  override def hashCode(): Int    = 121410467
  override def toString(): String = s"AlwaysEqual(${item.toString})"
}

object AlwaysEqual {
  def apply[A](item: A): AlwaysEqual[A]              = new AlwaysEqual(item)
  implicit def unwrap[A](wrapped: AlwaysEqual[A]): A = wrapped.get
  implicit def wrap[A](unwrapped: A): AlwaysEqual[A] = apply(unwrapped)

  implicit def alwaysEqualTypeMapper[B, A](
      implicit tm: TypeMapper[B, A]
  ): TypeMapper[B, AlwaysEqual[A]] =
    TypeMapper[B, AlwaysEqual[A]](b => AlwaysEqual(tm.toCustom(b)))(
      wrappedA => tm.toBase(wrappedA.get)
    )
}
