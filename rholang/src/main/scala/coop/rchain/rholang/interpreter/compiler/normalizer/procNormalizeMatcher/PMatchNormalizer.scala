package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.Applicative
import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.{Match, MatchCase, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.interpreter.errors.UnrecognizedNormalizerError
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Case, CaseImpl, PMatch, Proc}

import scala.collection.immutable.{BitSet, Vector}
import scala.collection.convert.ImplicitConversionsToScala._

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
      targetResult <- normalizeMatch[F](p.proc_, input.copy(par = VectorPar()))
      cases        <- p.listcase_.toList.traverse(liftCase)

      initAcc = (Vector[MatchCase](), targetResult.knownFree, BitSet(), false)
      casesResult <- cases.foldM(initAcc)(
                      (acc, caseImpl) =>
                        caseImpl match {
                          case (pattern, caseBody) => {
                            for {
                              patternResult <- normalizeMatch[F](
                                                pattern,
                                                ProcVisitInputs(
                                                  VectorPar(),
                                                  input.env.push,
                                                  DeBruijnLevelMap.empty
                                                )
                                              )
                              caseEnv    = input.env.absorbFree(patternResult.knownFree)
                              boundCount = patternResult.knownFree.countNoWildcards
                              caseBodyResult <- normalizeMatch[F](
                                                 caseBody,
                                                 ProcVisitInputs(VectorPar(), caseEnv, acc._2)
                                               )
                            } yield (
                              MatchCase(patternResult.par, caseBodyResult.par, boundCount) +: acc._1,
                              caseBodyResult.knownFree,
                              acc._3 | patternResult.par.locallyFree | caseBodyResult.par.locallyFree
                                .from(boundCount)
                                .map(x => x - boundCount),
                              acc._4 || caseBodyResult.par.connectiveUsed
                            )
                          }
                        }
                    )
    } yield ProcVisitOutputs(
      input.par.prepend(
        Match(
          targetResult.par,
          casesResult._1.reverse,
          casesResult._3 | targetResult.par.locallyFree,
          casesResult._4 || targetResult.par.connectiveUsed
        )
      ),
      casesResult._2
    )
  }
}
