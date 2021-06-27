package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.ConnOrBody
import coop.rchain.models.{Connective, ConnectiveBody, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  FreeMap,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PDisjunction

import scala.collection.immutable.Vector

object PDisjunctionNormalizer {
  def normalize[F[_]: Sync](p: PDisjunction, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      leftResult <- normalizeMatch[F](
                     p.proc_1,
                     ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                   )
      rightResult <- normalizeMatch[F](
                      p.proc_2,
                      ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                    )
      lp = leftResult.par
      resultConnective = lp.singleConnective() match {
        case Some(Connective(ConnOrBody(ConnectiveBody(ps)))) =>
          Connective(ConnOrBody(ConnectiveBody(ps :+ rightResult.par)))
        case _ =>
          Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
      }
    } yield ProcVisitOutputs(
      input.par.prepend(resultConnective, input.boundMapChain.depth),
      input.freeMap
        .addConnective(
          resultConnective.connectiveInstance,
          SourcePosition(p.line_num, p.col_num)
        )
    )
}
