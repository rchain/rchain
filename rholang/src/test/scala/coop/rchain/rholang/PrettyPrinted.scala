package coop.rchain.rholang
import org.scalacheck.Shrink

case class PrettyPrinted[T](value: T, toStr: T => String) {
  override def toString = toStr(value)
}

object PrettyPrinted {

  implicit def shrinkPretty[T](implicit shrinker: Shrink[T]): Shrink[PrettyPrinted[T]] = Shrink {
    p =>
      shrinker.shrink(p.value).map(PrettyPrinted[T](_, p.toStr))
  }
}
