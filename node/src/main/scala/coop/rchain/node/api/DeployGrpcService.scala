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
        BlockAPI.getBlockQueryResponse[F](q).toTask

      override def showBlocks(request: Empty): Observable[BlockInfo] =
        Observable
          .fromTask(BlockAPI.getBlocksResponse[F].toTask)
          .flatMap(b => Observable.fromIterable(b.blocks))

      override def listenForDataAtName(listeningName: Channel): Task[ListeningNameDataResponse] =
        BlockAPI.getListeningNameDataResponse[F](listeningName).toTask

      override def listenForContinuationAtName(
          listeningNames: Channels): Task[ListeningNameContinuationResponse] =
        BlockAPI.getListeningNameContinuationResponse[F](listeningNames).toTask
    }
}
