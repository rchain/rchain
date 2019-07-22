package coop.rchain.casper

import cats.Functor
import cats.syntax.functor._
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.catscontrib.TaskContrib.TaskOps
import monix.execution.Scheduler
import org.scalatest.{Assertion, Matchers}

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

  def effectTest[T](f: Effect[T])(implicit scheduler: Scheduler): T =
    f.unsafeRunSync(scheduler)
}
