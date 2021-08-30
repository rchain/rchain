package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PPar, Proc}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer
trait PParInstance {
  implicit def PParInstance[F[_]: Sync]
      : Normalizer[F, PPar, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PPar, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PPar, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] =
        Sync[F].suspend {
          for {
            result <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                       .normalize(p.proc_1, input)
            chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
            chainedRes <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                           .normalize(p.proc_2, chainedInput)
          } yield chainedRes
        }
    }
}
