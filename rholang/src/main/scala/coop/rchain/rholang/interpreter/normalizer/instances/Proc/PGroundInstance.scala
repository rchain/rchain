package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.MonadError
import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance.{GBool, GInt, GString, GUri}
import coop.rchain.models.rholang.implicits.ParExtension
import coop.rchain.models.{Expr, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  BoolFalse,
  BoolLiteral,
  BoolTrue,
  GroundBool,
  GroundInt,
  GroundString,
  GroundUri,
  PGround,
  Ground => AbsynGround
}
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.interpreter.errors.NormalizerError
import coop.rchain.rholang.interpreter.normalizer.Normalizer

import scala.util.Try
trait PGroundInstance {
  implicit def PGroundInstance[F[_]: Sync]
      : Normalizer[F, PGround, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PGround, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PGround, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        normalizeGround(p.ground_)
          .map(
            expr =>
              ProcVisitOutputs(
                input.par.prepend(expr, input.env.depth),
                input.knownFree
              )
          )

    }

}
