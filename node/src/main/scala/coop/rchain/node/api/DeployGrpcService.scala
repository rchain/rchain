package coop.rchain.node.api

import cats._
import com.google.protobuf.empty.Empty
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.protocol.{DeployData, DeployServiceGrpc, DeployServiceResponse, _}
import coop.rchain.casper.{MultiParentCasperRef, SafetyOracle}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.shared._

import scala.concurrent.Future

private[api] class DeployGrpcService[
    F[_]: Monad: MultiParentCasperRef: Log: Futurable: SafetyOracle: BlockStore]
    extends DeployServiceGrpc.DeployService {
  override def doDeploy(d: DeployData): Future[DeployServiceResponse] =
    BlockAPI.deploy[F](d).toFuture

  override def createBlock(e: Empty): Future[DeployServiceResponse] =
    BlockAPI.createBlock[F].toFuture

  override def addBlock(b: BlockMessage): Future[DeployServiceResponse] =
    BlockAPI.addBlock[F](b).toFuture

  override def showBlock(q: BlockQuery): Future[BlockQueryResponse] =
    BlockAPI.getBlockQueryResponse[F](q).toFuture

  override def showBlocks(e: Empty): Future[BlocksResponse] =
    BlockAPI.getBlocksResponse[F].toFuture
}
