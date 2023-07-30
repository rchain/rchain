package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Case, CaseImpl, PMatch, Proc}
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{FreeMap, ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.interpreter.errors.UnrecognizedNormalizerError

import scala.jdk.CollectionConverters._

object PMatchNormalizer {
  def normalize[F[_]: Sync](p: PMatch, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {

    def liftCase(c: Case): F[(Proc, Proc)] = c match {
      case ci: CaseImpl => Applicative[F].pure[(Proc, Proc)]((ci.proc_1, ci.proc_2))
      case _ =>
        Sync[F].raiseError(UnrecognizedNormalizerError("Unexpected Case implementation."))
    }

    for {
      targetResult <- normalizeMatch[F](p.proc_, input.copy(par = NilN()))
      cases        <- p.listcase_.asScala.toList.traverse(liftCase)

      initAcc = (Seq[MatchCaseN](), targetResult.freeMap)
      casesResult <- cases.foldM(initAcc)(
                      (acc, caseImpl) =>
                        caseImpl match {
                          case (pattern, caseBody) => {
                            for {
                              patternResult <- normalizeMatch[F](
                                                pattern,
                                                ProcVisitInputs(
                                                  NilN(),
                                                  input.boundMapChain.push,
                                                  FreeMap.empty
                                                )
                                              )
                              caseEnv    = input.boundMapChain.absorbFree(patternResult.freeMap)
                              boundCount = patternResult.freeMap.countNoWildcards
                              caseBodyResult <- normalizeMatch[F](
                                                 caseBody,
                                                 ProcVisitInputs(NilN(), caseEnv, acc._2)
                                               )
                            } yield (
                              MatchCaseN(
                                patternResult.par,
                                caseBodyResult.par,
                                boundCount
                              ) +: acc._1,
                              caseBodyResult.freeMap
                            )
                          }
                        }
                    )
    } yield {
      val m = MatchN(targetResult.par, casesResult._1.reverse)
      ProcVisitOutputs(input.par.combine(m), casesResult._2)
    }
  }
}
