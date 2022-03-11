package coop.rchain.catscontrib

import cats._, cats.data._, cats.syntax.all._

final class BooleanOps(self: Boolean) {

  final def either[A, B](a: => A): ConditionalEither[A] = new ConditionalEither(a)

  def fold[B](ifTrue: => B, ifFalse: => B): B = if (self) ifTrue else ifFalse

  final class ConditionalEither[A](a: => A) {
    def or[B](b: => B): Either[B, A] =
      if (self) a.asRight[B] else b.asLeft[A]
  }
}

trait ToBooleanOps {
  implicit def ToBooleanOpsFromBoolean(a: Boolean): BooleanOps = new BooleanOps(a)
}
