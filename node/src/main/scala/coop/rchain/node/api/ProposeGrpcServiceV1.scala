package coop.rchain.node.api

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol.propose.v1.{
  ProposeResponse,
  ProposeResultResponse,
  ProposeServiceV1GrpcMonix
}
import coop.rchain.casper.protocol.{
  PrintUnmatchedSendsQuery,
  ProposeQuery,
  ProposeResultQuery,
  ServiceError
}
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.casper.{
  LastFinalizedHeightConstraintChecker,
  ProposeFunction,
  SynchronyConstraintChecker
}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.StacksafeMessage
import coop.rchain.monix.Monixable
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler

object ProposeGrpcServiceV1 {

  def apply[F[_]: Monixable: Concurrent: BlockStore: EngineCell: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Log: Metrics: Span](
      triggerProposeFOpt: Option[ProposeFunction[F]],
      proposerStateRefOpt: Option[Ref[F, ProposerState[F]]]
  )(
      implicit worker: Scheduler
  ): ProposeServiceV1GrpcMonix.ProposeService =
    new ProposeServiceV1GrpcMonix.ProposeService {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): Task[R] =
        task.toTask
          .executeOn(worker)
          .fromTask
          .logOnError("Propose service method error.")
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )
          .toTask

      // This method should return immediately, only trggerred propose if allowed
      def propose(
          request: ProposeQuery
      ): Task[ProposeResponse] =
        defer(triggerProposeFOpt match {
          case Some(q) =>
            BlockAPI.createBlock[F](q, request.isAsync)
          case None => "Propose error: read-only node.".asLeft[String].pure[F]
        }) { r =>
          import ProposeResponse.Message
          import ProposeResponse.Message._
          ProposeResponse(r.fold[Message](Error, Result))
        }

      // This method waits for propose to finish, returning result data
      def proposeResult(request: ProposeResultQuery): Task[ProposeResultResponse] =
        defer(proposerStateRefOpt match {
          case Some(s) =>
            BlockAPI.getProposeResult[F](s)
          case None => "Error: read-only node.".asLeft[String].pure[F]
        }) { r =>
          import ProposeResultResponse.Message
          import ProposeResultResponse.Message._
          ProposeResultResponse(r.fold[Message](Error, Result))
        }
    }
}
