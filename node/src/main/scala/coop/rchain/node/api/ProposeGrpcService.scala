package coop.rchain.node.api

import cats.implicits._
import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.{TaskContrib, Taskable}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.either.{Either => GrpcEither}
import coop.rchain.models.StacksafeMessage
import coop.rchain.models.either.implicits._
import coop.rchain.shared._
import coop.rchain.metrics.Span
import monix.eval.Task
import monix.execution.Scheduler

private[api] object ProposeGrpcService {
  def instance[F[_]: Concurrent: Log: SafetyOracle: BlockStore: Taskable: Span: EngineCell](
      blockApiLock: Semaphore[F],
      tracing: Boolean
  )(
      implicit worker: Scheduler
  ): ProposeServiceGrpcMonix.ProposeService =
    new ProposeServiceGrpcMonix.ProposeService {

      private def defer[A <: StacksafeMessage[A]](
          task: F[Either[String, A]]
      ): Task[GrpcEither] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .executeWithOptions(TaskContrib.enableTracing(tracing))
          .attemptAndLog
          .attempt
          .map(_.fold(_.asLeft[A].toGrpcEither, _.toGrpcEither))

      override def propose(query: PrintUnmatchedSendsQuery): Task[GrpcEither] =
        defer(BlockAPI.createBlock[F](blockApiLock, Span.next, query.printUnmatchedSends))
    }
}
