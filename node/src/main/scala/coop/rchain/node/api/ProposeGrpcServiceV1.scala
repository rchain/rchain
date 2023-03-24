package coop.rchain.node.api

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.api.BlockApi
import coop.rchain.casper.protocol.propose.v1.{
  ProposeResponse,
  ProposeResultResponse,
  ProposeServiceFs2Grpc
}
import coop.rchain.casper.protocol.{ProposeQuery, ProposeResultQuery, ServiceError}
import coop.rchain.catscontrib.TaskContrib.AbstractTaskOps
import coop.rchain.models.StacksafeMessage
import coop.rchain.monix.Monixable
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared._
import io.grpc.Metadata

object ProposeGrpcServiceV1 {

  def apply[F[_]: Monixable: Sync: Log](
      blockApi: BlockApi[F]
  ): ProposeServiceFs2Grpc[F, Metadata] =
    new ProposeServiceFs2Grpc[F, Metadata] {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): F[R] =
        task
          .logOnError("Propose service method error.")
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )

      // This method should return immediately, only trggerred propose if allowed
      def propose(request: ProposeQuery, ctx: Metadata): F[ProposeResponse] =
        defer(blockApi.createBlock(request.isAsync)) { r =>
          import ProposeResponse.Message
          import ProposeResponse.Message._
          ProposeResponse(r.fold[Message](Error, Result))
        }

      // This method waits for propose to finish, returning result data
      def proposeResult(request: ProposeResultQuery, ctx: Metadata): F[ProposeResultResponse] =
        defer(blockApi.getProposeResult) { r =>
          import ProposeResultResponse.Message
          import ProposeResultResponse.Message._
          ProposeResultResponse(r.fold[Message](Error, Result))
        }
    }
}
