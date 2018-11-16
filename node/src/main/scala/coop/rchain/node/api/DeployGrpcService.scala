package coop.rchain.node.api

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.casper.protocol.{DeployData, DeployServiceResponse, _}
import coop.rchain.catscontrib.Taskable
import coop.rchain.shared._
import coop.rchain.catscontrib.TaskContrib._
import com.google.protobuf.empty.Empty
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

private[api] object DeployGrpcService {
  def instance[F[_]: Sync: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      implicit worker: Scheduler
  ): CasperMessageGrpcMonix.DeployService =
    new CasperMessageGrpcMonix.DeployService {

      private def defer[A](task: F[A]): Task[A] =
        Task.defer(task.toTask).executeOn(worker).attemptAndLog

      override def doDeploy(d: DeployData): Task[DeployServiceResponse] =
        defer(BlockAPI.deploy[F](d))

      override def createBlock(e: Empty): Task[DeployServiceResponse] =
        defer(BlockAPI.createBlock[F])

      override def addBlock(b: BlockMessage): Task[DeployServiceResponse] =
        defer(BlockAPI.addBlock[F](b))

      override def showBlock(q: BlockQuery): Task[BlockQueryResponse] =
        defer(BlockAPI.showBlock[F](q))

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
