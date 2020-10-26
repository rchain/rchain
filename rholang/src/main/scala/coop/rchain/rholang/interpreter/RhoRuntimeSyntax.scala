package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost

import scala.language.implicitConversions

trait RhoRuntimeSyntax {
  implicit final def rholangSyntaxRhoRuntime[F[_]: Sync](
      runtime: RhoRuntime[F]
  ): RhoRuntimeOps[F] =
    new RhoRuntimeOps[F](runtime)

}
final class RhoRuntimeOps[F[_]: Sync](
    private val runtime: RhoRuntime[F]
) {
  def evaluate(
      term: String,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult] =
    evaluate(term, Cost.UNSAFE_MAX, normalizerEnv)

  def evaluate(
      term: String
  ): F[EvaluateResult] =
    evaluate(term, Cost.UNSAFE_MAX, Map.empty)

  def evaluate(
      term: String,
      initialPhlo: Cost
  ): F[EvaluateResult] = evaluate(term, initialPhlo, Map.empty)

  def evaluate(
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(128)
    runtime.createSoftCheckpoint >>= { checkpoint =>
      runtime.evaluate(term, initialPhlo, normalizerEnv).attempt >>= {
        case Right(evaluateResult) =>
          if (evaluateResult.errors.nonEmpty)
            runtime.revertToSoftCheckpoint(checkpoint).as(evaluateResult)
          else evaluateResult.pure[F]
        case Left(throwable) =>
          runtime.revertToSoftCheckpoint(checkpoint) >> throwable
            .raiseError[F, EvaluateResult]
      }
    }
  }
}
