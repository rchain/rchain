package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{EMethod, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.PMethod
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

import scala.jdk.CollectionConverters._
import scala.collection.immutable.BitSet

object PMethodNormalizer {
  def normalize[F[_]: Sync](p: PMethod, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      targetResult <- normalizeMatch[F](p.proc_, input.copy(par = Par()))
      target       = targetResult.par
      initAcc = (
        List[Par](),
        ProcVisitInputs(Par(), input.boundMapChain, targetResult.freeMap),
        BitSet(),
        false
      )
      argResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)((acc, e) => {
                     normalizeMatch[F](e, acc._2).map(
                       procMatchResult =>
                         (
                           procMatchResult.par +: acc._1,
                           ProcVisitInputs(Par(), input.boundMapChain, procMatchResult.freeMap),
                           acc._3 | procMatchResult.par.locallyFree,
                           acc._4 || procMatchResult.par.connectiveUsed
                         )
                     )
                   })
    } yield ProcVisitOutputs(
      input.par.prepend(
        EMethod(
          p.var_,
          targetResult.par,
          argResults._1,
          target.locallyFree | argResults._3,
          target.connectiveUsed || argResults._4
        ),
        input.boundMapChain.depth
      ),
      argResults._2.freeMap
    )
}
