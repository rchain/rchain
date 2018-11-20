package coop.rchain.casper

import cats.Monad
import cats.syntax.functor._
import org.scalatest.{Assertion, Matchers}

object scalatestcontrib extends Matchers {
  implicit class AnyShouldF[F[_]: Monad, T](leftSideValue: F[T]) {
    def shouldBeF(value: T): F[Assertion] =
      leftSideValue.map(_ shouldBe value)
  }
}
