package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.Par
import coop.rchain.rholang.syntax._
import org.scalatest.Matchers._

object InterpreterUtil {
  def evaluate[F[_]: Sync](runtime: RhoRuntime[F], term: String)(
      implicit line: sourcecode.Line,
      file: sourcecode.File
  ): F[Unit] =
    runtime.evaluate(term).map {
      withClue(s"Evaluate was called at: ${file.value}:${line.value} and failed with: ") {
        _.errors shouldBe empty
      }
    }
}
