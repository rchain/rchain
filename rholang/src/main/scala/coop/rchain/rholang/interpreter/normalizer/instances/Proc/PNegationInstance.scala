package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.ConnNotBody
import coop.rchain.models.{Connective, Par}
import coop.rchain.models.rholang.implicits.{ParExtension, VectorPar}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PNegation, Proc}
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, SourcePosition}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer
trait PNegationInstance {
  implicit def PNegationInstance[F[_]: Sync]
      : Normalizer[F, PNegation, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PNegation, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PNegation, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] =
        Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
          .normalize(
            p.proc_,
            ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
          )
          .map(
            bodyResult =>
              ProcVisitOutputs(
                input.par.prepend(Connective(ConnNotBody(bodyResult.par)), input.env.depth),
                input.knownFree
                  .addConnective(
                    ConnNotBody(bodyResult.par),
                    SourcePosition(p.line_num, p.col_num)
                  )
              )
          )
    }
}
