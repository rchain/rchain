package coop.rchain.node.api

import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine._
import EngineCell._
import coop.rchain.casper.{
  LastFinalizedHeightConstraintChecker,
  SafetyOracle,
  SynchronyConstraintChecker
}
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.protocol.{PrintUnmatchedSendsQuery, ServiceError}
import coop.rchain.casper.protocol.propose.v1.{ProposeResponse, ProposeServiceV1GrpcMonix}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.StacksafeMessage
import coop.rchain.shared._
import coop.rchain.shared.ThrowableOps._
import monix.eval.Task
import monix.execution.Scheduler

object ProposeGrpcServiceV1 {
  def instance[F[_]: Concurrent: Log: SafetyOracle: BlockStore: Metrics: Taskable: Span: EngineCell: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker](
      blockApiLock: Semaphore[F]
  )(
      implicit worker: Scheduler
  ): ProposeServiceV1GrpcMonix.ProposeService =
    new ProposeServiceV1GrpcMonix.ProposeService {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): Task[R] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )

      def propose(request: PrintUnmatchedSendsQuery): Task[ProposeResponse] =
        defer(BlockAPI.createBlock[F](blockApiLock, request.printUnmatchedSends)) { r =>
          import ProposeResponse.Message
          import ProposeResponse.Message._
          ProposeResponse(r.fold[Message](Error, Result))
        }
    }
}
