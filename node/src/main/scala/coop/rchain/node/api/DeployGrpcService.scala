package coop.rchain.node.api

import cats._
import cats.data._
import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import cats.implicits._
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

private[api] object DeployGrpcService {
  def instance[F[_]: Concurrent: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      blockApiLock: Semaphore[F]
  )(
      implicit worker: Scheduler
  ): CasperMessageGrpcMonix.DeployService =
    new CasperMessageGrpcMonix.DeployService {

      private def defer[A <: StacksafeMessage[A]](
          task: F[Either[String, A]]
      ): Task[GrpcEither] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .attempt
          .map(_.fold(_.asLeft[A].toGrpcEither, _.toGrpcEither))

      private def deferList[A <: StacksafeMessage[A]](task: F[List[A]]): Task[List[GrpcEither]] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .attempt
          .map(_.fold(t => List(t.asLeft[A].toGrpcEither), _.map(_.asRight[String].toGrpcEither)))

      override def doDeploy(d: DeployData): Task[GrpcEither] =
        defer(BlockAPI.deploy[F](d))

      override def createBlock(e: Empty): Task[GrpcEither] =
        defer(BlockAPI.createBlock[F](blockApiLock))

      override def showBlock(q: BlockQuery): Task[GrpcEither] =
        defer(BlockAPI.showBlock[F](q))

      override def visualizeDag(q: VisualizeDagQuery): Task[GrpcEither] = {
        type Effect[A] = StateT[Id, StringBuffer, A]
        implicit val ser: GraphSerializer[Effect]       = new StringSerializer[Effect]
        val stringify: Effect[Graphz[Effect]] => String = _.runS(new StringBuffer).toString

        val depth  = if (q.depth <= 0) None else Some(q.depth)
        val config = GraphConfig(q.showJustificationLines)

        defer(
          BlockAPI
            .visualizeDag[F, Effect](
              depth,
              (ts, lfb) => GraphzGenerator.dagAsCluster[F, Effect](ts, lfb, config),
              stringify
            )
        )
      }

      override def showBlocks(request: BlocksQuery): Observable[GrpcEither] =
        Observable
          .fromTask(deferList(BlockAPI.showBlocks[F](request.depth)))
          .flatMap(Observable.fromIterable)

      override def listenForDataAtName(request: DataAtNameQuery): Task[GrpcEither] =
        defer(BlockAPI.getListeningNameDataResponse[F](request.depth, request.name.get))

      override def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[GrpcEither] =
        defer(BlockAPI.getListeningNameContinuationResponse[F](request.depth, request.names))

      override def showMainChain(request: BlocksQuery): Observable[GrpcEither] =
        Observable
          .fromTask(deferList(BlockAPI.showMainChain[F](request.depth)))
          .flatMap(Observable.fromIterable)

      override def findBlockWithDeploy(request: FindDeployInBlockQuery): Task[GrpcEither] =
        defer(BlockAPI.findBlockWithDeploy[F](request.user, request.timestamp))

      override def previewPrivateNames(
          request: PrivateNamePreviewQuery
      ): Task[GrpcEither] =
        defer(BlockAPI.previewPrivateNames[F](request.user, request.timestamp, request.nameQty))
    }
}
