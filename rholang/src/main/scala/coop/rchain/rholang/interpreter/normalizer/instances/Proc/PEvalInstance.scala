package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.ParExtension
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, PEval}
import coop.rchain.rholang.interpreter.compiler.{
  NameVisitInputs,
  NameVisitOutputs,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.interpreter.normalizer.Normalizer

trait PEvalInstance {
  implicit def PEvalInstance[F[_]: Sync]
      : Normalizer[F, PEval, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PEval, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PEval, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par]
          .normalize(p.name_, NameVisitInputs(input.env, input.knownFree))
          .map(
            nameMatchResult =>
              ProcVisitOutputs(
                input.par ++ nameMatchResult.chan,
                nameMatchResult.knownFree
              )
          )
    }
}
