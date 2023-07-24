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
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._

object PMethodNormalizer {
  def normalize[F[_]: Sync](p: PMethod, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      targetResult <- normalizeMatch[F](p.proc_, input.copy(toProto(NilN())))
      target       = fromProto(targetResult.par)
      initAcc = (
        Seq[ParN](),
        ProcVisitInputs(toProto(NilN()), input.boundMapChain, targetResult.freeMap)
      )
      argResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)((acc, e) => {
                     normalizeMatch[F](e, acc._2).map(
                       procMatchResult =>
                         (
                           fromProto(procMatchResult.par) +: acc._1,
                           ProcVisitInputs(
                             toProto(NilN()),
                             input.boundMapChain,
                             procMatchResult.freeMap
                           )
                         )
                     )
                   })
    } yield {
      val inpPar = fromProto(input.par)
      val method = EMethodN(p.var_, target, argResults._1)
      ProcVisitOutputs(toProto(inpPar.add(method)), argResults._2.freeMap)
    }
}
