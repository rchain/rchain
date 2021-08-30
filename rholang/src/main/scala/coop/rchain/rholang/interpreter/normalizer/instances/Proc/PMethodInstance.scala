package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.{EMethod, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PMethod, Proc}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.models.rholang.implicits._
import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.immutable.BitSet

trait PMethodInstance {
  implicit def PMethodInstance[F[_]: Sync]
      : Normalizer[F, PMethod, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PMethod, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PMethod, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        for {
          targetResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                           .normalize(p.proc_, input.copy(par = Par()))
          target = targetResult.par
          initAcc = (
            List[Par](),
            ProcVisitInputs(Par(), input.env, targetResult.knownFree),
            BitSet(),
            false
          )
          argResults <- p.listproc_.toList.reverse.foldM(initAcc)((acc, e) => {
                         Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                           .normalize(e, acc._2)
                           .map(
                             procMatchResult =>
                               (
                                 procMatchResult.par +: acc._1,
                                 ProcVisitInputs(Par(), input.env, procMatchResult.knownFree),
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
            input.env.depth
          ),
          argResults._2.knownFree
        )
    }
}
