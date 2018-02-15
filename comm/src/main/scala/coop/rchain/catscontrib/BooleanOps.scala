package coop.rchain.catscontrib

final class BooleanOps(self: Boolean) {

  final def either[A, B](a: => A): ConditionalEither[A] = new ConditionalEither(a)

  final class ConditionalEither[A](a: => A) {
    def or[B](b: => B): Either[B, A] =
      if (self) Right(a) else Left(b)
  }
}

trait ToBooleanOps {
  implicit def ToBooleanOpsFromBoolean(a: Boolean): BooleanOps = new BooleanOps(a)
}
