package coop.rchain.shared

import cats.Functor
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.functor._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

object scalatestcontrib extends Matchers {

  implicit class AnyShouldF[F[_]: Functor, T](leftSideValue: F[T]) {
    def shouldBeF(value: T)(implicit file: sourcecode.File, line: sourcecode.Line): F[Assertion] =
      leftSideValue.map(
        x =>
          withClue(s"Assertion failed at ${file.value}:${line.value}:\n\n") {
            x shouldBe value
          }
      )
  }

  def effectTest[T](f: IO[T]): T = f.unsafeRunSync
}
