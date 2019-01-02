package coop.rchain.node.api

import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.protocol.{DeployData, DeployServiceResponse, _}
import coop.rchain.shared._
import coop.rchain.graphz._
import coop.rchain.casper.api.{GraphConfig, GraphzGenerator}
import com.google.protobuf.empty.Empty

import cats.mtl._
import cats.mtl.implicits._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.{Taskable, ToAbstractContext}
import coop.rchain.catscontrib.TaskContrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

private[api] object DeployGrpcService {
  def instance[F[_]: Concurrent: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable: ToAbstractContext](
      blockApiLock: Semaphore[F]
  )(
      implicit worker: Scheduler
  ): CasperMessageGrpcMonix.DeployService =
    new CasperMessageGrpcMonix.DeployService {

      private def defer[A](task: F[A]): Task[A] =
        Task.defer(task.toTask).executeOn(worker).attemptAndLog

      override def doDeploy(d: DeployData): Task[DeployServiceResponse] =
        defer(BlockAPI.deploy[F](d))

      override def createBlock(e: Empty): Task[DeployServiceResponse] =
        defer(BlockAPI.createBlock[F](blockApiLock))

      override def showBlock(q: BlockQuery): Task[BlockQueryResponse] =
        defer(BlockAPI.showBlock[F](q))

      // TODO handle potentiall errors (at least by returning proper response)
      override def visualizeBlocks(q: BlocksQuery): Task[VisualizeBlocksResponse] = {
        type Effect[A] = StateT[Id, StringBuffer, A]
        implicit val ser: StringSerializer[Effect]      = new StringSerializer[Effect]
        val stringify: Effect[Graphz[Effect]] => String = _.runS(new StringBuffer).toString

        val depth = if (q.depth <= 0) None else Some(q.depth)

        defer(
          BlockAPI
            .visualizeDag[F, Effect](
              depth,
              (ts, lfb) => GraphzGenerator.dagAsCluster[F, Effect](ts, lfb, GraphConfig()),
              stringify
            )
            .map(graph => VisualizeBlocksResponse(graph))
        )
      }

      override def showBlocks(request: BlocksQuery): Observable[BlockInfoWithoutTuplespace] =
        Observable
          .fromTask(defer(BlockAPI.showBlocks[F](request.depth)))
          .flatMap(Observable.fromIterable)

      // TODO: Handle error case
      override def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
        defer(BlockAPI.getListeningNameDataResponse[F](request.depth, request.name.get))

      override def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[ListeningNameContinuationResponse] =
        defer(BlockAPI.getListeningNameContinuationResponse[F](request.depth, request.names))

      override def showMainChain(request: BlocksQuery): Observable[BlockInfoWithoutTuplespace] =
        Observable
          .fromTask(defer(BlockAPI.showMainChain[F](request.depth)))
          .flatMap(Observable.fromIterable)

      override def findBlockWithDeploy(request: FindDeployInBlockQuery): Task[BlockQueryResponse] =
        defer(BlockAPI.findBlockWithDeploy[F](request.user, request.timestamp))

      override def previewPrivateNames(
          request: PrivateNamePreviewQuery
      ): Task[PrivateNamePreviewResponse] =
        defer(BlockAPI.previewPrivateNames[F](request.user, request.timestamp, request.nameQty))
    }
}
