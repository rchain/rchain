package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.{Par, Receive, ReceiveBind}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, PContr, Proc}
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, VarSort}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.compiler.Visit._

import scala.collection.immutable.{BitSet, Vector}
import scala.collection.convert.ImplicitConversionsToScala._

trait PContrInstance {
  implicit def PContrInstance[F[_]: Sync]
      : Normalizer[F, PContr, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PContr, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PContr, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        // A free variable can only be used once in any of the parameters.
        // And we start with the empty free variable map because these free
        // variables aren't free in the surrounding context: they're binders
        for {
          nameMatchResult <- Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par].normalize(
                              p.name_,
                              NameVisitInputs(input.env, input.knownFree)
                            )
          initAcc = (Vector[Par](), DeBruijnLevelMap.empty[VarSort], BitSet())
          // Note that we go over these in the order they were given and reverse
          // down below. This is because it makes more sense to number the free
          // variables in the order given, rather than in reverse.
          formalsResults <- p.listname_.toList.foldM(initAcc)(
                             (acc, n: Name) => {
                               Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par]
                                 .normalize(
                                   n,
                                   NameVisitInputs(input.env.push, acc._2)
                                 )
                                 .flatMap { res =>
                                   failOnInvalidConnective(input, input.env.depth, res)
                                     .fold(
                                       err => Sync[F].raiseError[NameVisitOutputs](err),
                                       _.pure[F]
                                     )
                                 }
                                 .map(
                                   result =>
                                     (
                                       result.chan +: acc._1,
                                       result.knownFree,
                                       acc._3 | ParLocallyFree
                                         .locallyFree(result.chan, input.env.depth + 1)
                                     )
                                 )
                             }
                           )
          remainderResult <- normalizeMatchName[F](p.nameremainder_, formalsResults._2)
          newEnv          = input.env.absorbFree(remainderResult._2)
          boundCount      = remainderResult._2.countNoWildcards
          bodyResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(
                         p.proc_,
                         ProcVisitInputs(VectorPar(), newEnv, nameMatchResult.knownFree)
                       )
        } yield ProcVisitOutputs(
          input.par.prepend(
            Receive(
              binds = List(
                ReceiveBind(
                  formalsResults._1.reverse,
                  nameMatchResult.chan,
                  remainderResult._1,
                  boundCount
                )
              ),
              body = bodyResult.par,
              persistent = true,
              peek = false,
              bindCount = boundCount,
              locallyFree = ParLocallyFree
                .locallyFree(nameMatchResult.chan, input.env.depth) | formalsResults._3
                | (bodyResult.par.locallyFree
                  .from(boundCount)
                  .map(x => x - boundCount)),
              connectiveUsed = ParLocallyFree
                .connectiveUsed(nameMatchResult.chan) || bodyResult.par.connectiveUsed
            )
          ),
          bodyResult.knownFree
        )
    }
}
