package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PMethod
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

import scala.jdk.CollectionConverters._

object PMethodNormalizer {
  def normalize[F[_]: Sync](p: PMethod, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      targetResult <- normalizeMatch[F](p.proc_, input.copy(NilN))
      target       = targetResult.par
      initAcc = (
        Vector[ParN](),
        ProcVisitInputs(NilN, input.boundMapChain, targetResult.freeMap)
      )
      argResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)((acc, e) => {
                     normalizeMatch[F](e, acc._2).map(
                       procMatchResult =>
                         (
                           procMatchResult.par +: acc._1,
                           ProcVisitInputs(
                             NilN,
                             input.boundMapChain,
                             procMatchResult.freeMap
                           )
                         )
                     )
                   })
    } yield {
      val method = EMethodN(p.var_, target, argResults._1)
      ProcVisitOutputs(ParN.combine(input.par, method), argResults._2.freeMap)
    }
}
