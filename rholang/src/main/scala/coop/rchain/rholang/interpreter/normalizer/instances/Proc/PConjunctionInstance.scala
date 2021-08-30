package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.ConnAndBody
import coop.rchain.models.rholang.implicits.{ParExtension, VectorPar}
import coop.rchain.models.{Connective, ConnectiveBody, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PConjunction, Proc}
import coop.rchain.rholang.interpreter.compiler.SourcePosition
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.compiler.Visit._

import scala.collection.immutable.Vector

trait PConjunctionInstance {
  implicit def PConjunctionInstance[F[_]: Sync]
      : Normalizer[F, PConjunction, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PConjunction, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PConjunction, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        for {
          leftResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(
                         p.proc_1,
                         ProcVisitInputs(VectorPar(), input.env, input.knownFree)
                       )
          rightResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.env, leftResult.knownFree)
                        )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnAndBody(ConnectiveBody(ps)))) =>
              Connective(ConnAndBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.env.depth),
          rightResult.knownFree
            .addConnective(
              resultConnective.connectiveInstance,
              SourcePosition(p.line_num, p.col_num)
            )
        )
    }
}
