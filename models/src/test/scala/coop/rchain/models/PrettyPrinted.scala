package coop.rchain.models

import org.scalacheck.{Arbitrary, Shrink}

case class PrettyPrinted[T](value: T, toStr: T => String) {
  override def toString = toStr(value)
}

object PrettyPrinted {

  implicit def arbitraryPrettyPrintedFromPretty[T: Arbitrary: Pretty]
    : Arbitrary[PrettyPrinted[T]] = {
    val gen = implicitly[Arbitrary[T]].arbitrary
    Arbitrary(gen.map(PrettyPrinted(_, Pretty[T].pretty(_, 0))))
  }

  implicit def shrinkPrettyPrinted[T](implicit shrinker: Shrink[T]): Shrink[PrettyPrinted[T]] =
    Shrink { p =>
      shrinker.shrink(p.value).map(PrettyPrinted[T](_, p.toStr))
    }
}
