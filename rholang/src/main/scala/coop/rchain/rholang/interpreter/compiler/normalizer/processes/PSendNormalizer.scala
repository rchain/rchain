package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn.Bindings._
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PSend, SendMultiple, SendSingle}
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.normalizer.NameNormalizeMatcher
import coop.rchain.rholang.interpreter.compiler.{NameVisitInputs, ProcVisitInputs, ProcVisitOutputs}

import scala.jdk.CollectionConverters._

object PSendNormalizer {
  def normalize[F[_]: Sync](p: PSend, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      nameMatchResult <- NameNormalizeMatcher.normalizeMatch[F](
                          p.name_,
                          NameVisitInputs(input.boundMapChain, input.freeMap)
                        )
      initAcc = (
        Seq[ParN](),
        ProcVisitInputs(NilN, input.boundMapChain, nameMatchResult.freeMap)
      )
      dataResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)(
                      (acc, e) => {
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
                      }
                    )
      persistent = p.send_ match {
        case _: SendSingle   => false
        case _: SendMultiple => true
      }
      send = SendN(nameMatchResult.par, dataResults._1, persistent)
      par  = ParN.combine(input.par, send)
    } yield ProcVisitOutputs(
      par,
      dataResults._2.freeMap
    )
}
