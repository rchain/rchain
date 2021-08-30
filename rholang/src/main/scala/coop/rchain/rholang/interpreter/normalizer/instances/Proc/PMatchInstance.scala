package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Match, MatchCase, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Case, CaseImpl, PMatch, Proc}
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.interpreter.errors.UnrecognizedNormalizerError
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import scala.collection.convert.ImplicitConversionsToScala._

import scala.collection.immutable.{BitSet, Vector}
trait PMatchInstance {
  implicit def PMatchInstance[F[_]: Sync]
      : Normalizer[F, PMatch, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PMatch, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PMatch, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] = {

        def liftCase(c: Case): F[(Proc, Proc)] = c match {
          case ci: CaseImpl => Applicative[F].pure[(Proc, Proc)]((ci.proc_1, ci.proc_2))
          case _ =>
            Sync[F].raiseError(UnrecognizedNormalizerError("Unexpected Case implementation."))
        }

        for {
          targetResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                           .normalize(p.proc_, input.copy(par = VectorPar()))
          cases <- p.listcase_.toList.traverse(liftCase)

          initAcc = (Vector[MatchCase](), targetResult.knownFree, BitSet(), false)
          casesResult <- cases.foldM(initAcc)(
                          (acc, caseImpl) =>
                            caseImpl match {
                              case (pattern, caseBody) => {
                                for {
                                  patternResult <- Normalizer[
                                                    F,
                                                    Proc,
                                                    ProcVisitInputs,
                                                    ProcVisitOutputs,
                                                    Par
                                                  ].normalize(
                                                    pattern,
                                                    ProcVisitInputs(
                                                      VectorPar(),
                                                      input.env.push,
                                                      DeBruijnLevelMap.empty
                                                    )
                                                  )
                                  caseEnv    = input.env.absorbFree(patternResult.knownFree)
                                  boundCount = patternResult.knownFree.countNoWildcards
                                  caseBodyResult <- Normalizer[
                                                     F,
                                                     Proc,
                                                     ProcVisitInputs,
                                                     ProcVisitOutputs,
                                                     Par
                                                   ].normalize(
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
}
