package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, PContr}
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.compiler.normalizer.{
  NameNormalizeMatcher,
  RemainderNormalizeMatcher
}

import scala.jdk.CollectionConverters._

object PContrNormalizer {
  def normalize[F[_]: Sync](p: PContr, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    // A free variable can only be used once in any of the parameters.
    // And we start with the empty free variable map because these free
    // variables aren't free in the surrounding context: they're binders
    for {
      nameMatchResult <- NameNormalizeMatcher
                          .normalizeMatch[F](
                            p.name_,
                            NameVisitInputs(input.boundMapChain, input.freeMap)
                          )
      initAcc = (Vector[ParN](), FreeMap.empty[VarSort])
      // Note that we go over these in the order they were given and reverse
      // down below. This is because it makes more sense to number the free
      // variables in the order given, rather than in reverse.
      formalsResults <- p.listname_.asScala.toList.foldM(initAcc)(
                         (acc, n: Name) => {
                           NameNormalizeMatcher
                             .normalizeMatch[F](
                               n,
                               NameVisitInputs(input.boundMapChain.push, acc._2)
                             )
                             .flatMap { res =>
                               Utils
                                 .failOnInvalidConnective(input, res)
                                 .fold(
                                   err => Sync[F].raiseError[NameVisitOutputs](err),
                                   _.pure[F]
                                 )
                             }
                             .map(
                               result =>
                                 (
                                   result.par +: acc._1,
                                   result.freeMap
                                 )
                             )
                         }
                       )
      remainderResult <- RemainderNormalizeMatcher
                          .normalizeMatchName[F](p.nameremainder_, formalsResults._2)
      newEnv     = input.boundMapChain.absorbFree(remainderResult._2)
      boundCount = remainderResult._2.countNoWildcards
      bodyResult <- ProcNormalizeMatcher.normalizeMatch[F](
                     p.proc_,
                     ProcVisitInputs(NilN, newEnv, nameMatchResult.freeMap)
                   )
    } yield {
      val newReceive = ReceiveN(
        ReceiveBindN(
          formalsResults._1.reverse,
          nameMatchResult.par,
          remainderResult._1,
          boundCount
        ),
        body = bodyResult.par,
        persistent = true,
        peek = false,
        bindCount = boundCount
      )
      ProcVisitOutputs(
        input.par.combine(newReceive),
        bodyResult.freeMap
      )
    }
}
