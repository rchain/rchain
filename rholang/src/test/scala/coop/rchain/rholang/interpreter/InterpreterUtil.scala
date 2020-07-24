package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.syntax._
import org.scalatest.Matchers._

object InterpreterUtil {
  def evaluate[F[_]: Sync](runtime: Runtime[F], term: String)(
      implicit line: sourcecode.Line,
      file: sourcecode.File
  ): F[Unit] = {
    implicit val c                 = runtime.cost
    implicit val i: Interpreter[F] = Interpreter.newIntrepreter[F]
    Interpreter[F].evaluate(runtime, term).map {
      withClue(s"Evaluate was called at: ${file.value}:${line.value} and failed with: ") {
        _.errors shouldBe empty
      }
    }
  }
}
