package coop.rchain.node.api

import cats._
import cats.data._
import cats.implicits._
import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import cats.mtl.implicits._

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api._
import coop.rchain.casper.protocol.{DeployData, _}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.either.{Either => GrpcEither}
import coop.rchain.graphz._
import coop.rchain.models.StacksafeMessage
import coop.rchain.models.either.implicits._
import coop.rchain.shared._

import com.google.protobuf.empty.Empty
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

private[api] object ProposeGrpcService {
  def instance[F[_]: Concurrent: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      blockApiLock: Semaphore[F]
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
          .attemptAndLog
          .attempt
          .map(_.fold(_.asLeft[A].toGrpcEither, _.toGrpcEither))

      override def propose(e: Empty): Task[GrpcEither] =
        defer(BlockAPI.createBlock[F](blockApiLock))
    }
}
