package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.ConnAndBody
import coop.rchain.models.{Connective, ConnectiveBody, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs, SourcePosition}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PConjunction

import scala.collection.immutable.Vector

object PConjunctionNormalizer {
  def normalize[F[_]: Sync](p: PConjunction, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      leftResult <- normalizeMatch[F](
                     p.proc_1,
                     ProcVisitInputs(VectorPar(), input.env, input.knownFree)
                   )
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(VectorPar(), input.env, leftResult.knownFree)
                    )
      lp = leftResult.par
      resultConnective = lp.singleConnective() match {
        case Some(Connective(ConnAndBody(ConnectiveBody(ps)))) =>
          Connective(ConnAndBody(ConnectiveBody(ps :+ rightResult.par)))
        case _ =>
          Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
      }
    } yield ProcVisitOutputs(
      input.par.prepend(resultConnective, input.env.depth),
      rightResult.knownFree
        .addConnective(
          resultConnective.connectiveInstance,
          SourcePosition(p.line_num, p.col_num)
        )
    )
}
