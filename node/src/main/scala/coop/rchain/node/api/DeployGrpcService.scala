package coop.rchain.node.api

import cats.effect.Sync

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.casper.protocol.{DeployData, DeployServiceResponse, _}
import coop.rchain.catscontrib.Taskable
import coop.rchain.models.Channel
import coop.rchain.shared._

import com.google.protobuf.empty.Empty
import monix.eval.Task
import monix.reactive.Observable

private[api] object DeployGrpcService {
  def instance[F[_]: Sync: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable]
    : CasperMessageGrpcMonix.DeployService =
    new CasperMessageGrpcMonix.DeployService {

      override def doDeploy(d: DeployData): Task[DeployServiceResponse] =
        BlockAPI.deploy[F](d).toTask

      override def createBlock(e: Empty): Task[DeployServiceResponse] =
        BlockAPI.createBlock[F].toTask

      override def addBlock(b: BlockMessage): Task[DeployServiceResponse] =
        BlockAPI.addBlock[F](b).toTask

      override def showBlock(q: BlockQuery): Task[BlockQueryResponse] =
        BlockAPI.showBlock[F](q).toTask

      override def showMainChain(request: BlocksQuery): Observable[BlockInfoWithoutTuplespace] =
        Observable
          .fromTask(BlockAPI.showMainChain[F](request.depth).toTask)
          .flatMap(Observable.fromIterable)

      // TODO: Handle error case
      override def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
        BlockAPI.getListeningNameDataResponse[F](request.depth, request.channel.get).toTask

      override def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[ListeningNameContinuationResponse] =
        BlockAPI.getListeningNameContinuationResponse[F](request.depth, request.channels).toTask
    }
}
