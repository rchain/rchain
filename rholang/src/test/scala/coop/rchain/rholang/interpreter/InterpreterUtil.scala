package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.rholang.interpreter.accounting.{_cost, Cost}
import org.scalatest.Matchers._

object InterpreterUtil {

  def evaluate[F[_]: Sync: _cost](runtime: Runtime[F], term: String)(
      implicit line: sourcecode.Line,
      file: sourcecode.File
  ): F[Unit] =
    Interpreter[F].evaluate(runtime, term).map {
      withClue(s"Evaluate was called at: ${file.value}:${line.value} and failed with: ") {
        _.errors shouldBe empty
      }
    }
}
