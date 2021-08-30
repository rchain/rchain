package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.ConnOrBody
import coop.rchain.models.rholang.implicits.{ParExtension, VectorPar}
import coop.rchain.models.{Connective, ConnectiveBody, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PDisjunction, Proc}
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, SourcePosition}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.compiler.Visit._

import scala.collection.immutable.Vector

trait PDisjunctionInstance {
  implicit def PDisjunctionInstance[F[_]: Sync]
      : Normalizer[F, PDisjunction, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PDisjunction, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PDisjunction, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        for {
          leftResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                         .normalize(
                           p.proc_1,
                           ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
                         )
          rightResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                          .normalize(
                            p.proc_2,
                            ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
                          )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnOrBody(ConnectiveBody(ps)))) =>
              Connective(ConnOrBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.env.depth),
          input.knownFree
            .addConnective(
              resultConnective.connectiveInstance,
              SourcePosition(p.line_num, p.col_num)
            )
        )
    }
}
