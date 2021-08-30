package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{EMatches, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PMatches, Proc}
import coop.rchain.rholang.interpreter.compiler.DeBruijnLevelMap
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer
trait PMatchesInstance {
  implicit def PMatchesInstance[F[_]: Sync]
      : Normalizer[F, PMatches, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PMatches, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PMatches, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] = // In case of 'matches' expression the free variables from the pattern are thrown away
        // and only the ones from the target are used.
        // This is because the "target matches pattern" should have the same semantics as
        // "match target { pattern => true ; _ => false}
        // so free variables from pattern should not be visible at the top level
        for {
          leftResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                         .normalize(p.proc_1, input.copy(par = VectorPar()))
          rightResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                          .normalize(
                            p.proc_2,
                            ProcVisitInputs(
                              VectorPar(),
                              input.env.push,
                              DeBruijnLevelMap.empty
                            )
                          )
        } yield ProcVisitOutputs(
          input.par.prepend(EMatches(leftResult.par, rightResult.par), input.env.depth),
          leftResult.knownFree
        )
    }
}
