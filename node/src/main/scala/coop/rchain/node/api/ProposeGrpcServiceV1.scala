package coop.rchain.node.api

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.api.BlockApi
import coop.rchain.casper.protocol.propose.v1.{
  ProposeResponse,
  ProposeResultResponse,
  ProposeServiceV1GrpcMonix
}
import coop.rchain.casper.protocol.{ProposeQuery, ProposeResultQuery, ServiceError}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.models.StacksafeMessage
import coop.rchain.monix.Monixable
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler

object ProposeGrpcServiceV1 {

  def apply[F[_]: Monixable: Sync: Log](
      blockApi: BlockApi[F]
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
        defer(blockApi.createBlock(request.isAsync)) { r =>
          import ProposeResponse.Message
          import ProposeResponse.Message._
          ProposeResponse(r.fold[Message](Error, Result))
        }

      // This method waits for propose to finish, returning result data
      def proposeResult(request: ProposeResultQuery): Task[ProposeResultResponse] =
        defer(blockApi.getProposeResult) { r =>
          import ProposeResultResponse.Message
          import ProposeResultResponse.Message._
          ProposeResultResponse(r.fold[Message](Error, Result))
        }
    }
}
