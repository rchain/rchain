package coop.rchain.node.api

import cats._
import com.google.protobuf.empty.Empty
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.protocol._
import coop.rchain.casper.{MultiParentCasperConstructor, SafetyOracle}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.models.Channel
import coop.rchain.shared._
import io.grpc.stub.StreamObserver
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext

private[api] class DeployGrpcService[
    F[_]: Monad: Capture: MultiParentCasperConstructor: Log: Futurable: SafetyOracle: BlockStore](
    implicit ev: ExecutionContext)
    extends DeployServiceGrpc.DeployService {
  override def doDeploy(d: DeployData): Future[DeployServiceResponse] =
    BlockAPI.deploy[F](d).toFuture

  override def createBlock(e: Empty): Future[DeployServiceResponse] =
    BlockAPI.createBlock[F].toFuture

  override def addBlock(b: BlockMessage): Future[DeployServiceResponse] =
    BlockAPI.addBlock[F](b).toFuture

  override def showBlock(q: BlockQuery): Future[BlockQueryResponse] =
    BlockAPI.getBlockQueryResponse[F](q).toFuture

  override def showBlocks(request: Empty, observer: StreamObserver[BlockInfo]): Unit =
    BlockAPI.getBlocksResponse[F].toFuture.onComplete {
      case Success(blockResponse) =>
        blockResponse.blocks.foreach(bi => observer.onNext(bi))
        observer.onCompleted()
      case Failure(ex) => observer.onError(ex)
    }

  override def listenForDataAtName(listeningName: Channel): Future[ListeningNameDataResponse] =
    BlockAPI.getListeningNameDataResponse[F](listeningName).toFuture

  override def listenForContinuationAtName(
      listeningNames: Channels): Future[ListeningNameContinuationResponse] =
    BlockAPI.getListeningNameContinuationResponse[F](listeningNames).toFuture
}
