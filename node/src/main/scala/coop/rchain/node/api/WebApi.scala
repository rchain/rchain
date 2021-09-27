package coop.rchain.node.api

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{Casper, ProposeFunction}
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.api.BlockAPI.LatestBlockMessageError
import coop.rchain.casper.blocks.proposer.ProposerResult
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.{BlockInfo, DataWithBlockInfo, DeployData, LightBlockInfo}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{SignaturesAlg, Signed}
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.GUnforgeable.UnfInstance.{GDeployIdBody, GDeployerIdBody, GPrivateBody}
import coop.rchain.models._
import coop.rchain.node.api.WebApi._
import coop.rchain.shared.Log
import coop.rchain.state.StateManager
import fs2.concurrent.Queue

trait WebApi[F[_]] {
  def status: F[ApiStatus]

  // Get data to create deploy
  def prepareDeploy(request: Option[PrepareRequest]): F[PrepareResponse]

  // Write data (deploy)
  def deploy(request: DeployRequest): F[String]

  // Read data (listen)
  def listenForDataAtName(request: DataRequest): F[DataResponse]

  // Blocks info

  def lastFinalizedBlock: F[BlockInfo]

  def getBlock(hash: String): F[BlockInfo]

  def getBlocks(depth: Int): F[List[LightBlockInfo]]

  def findDeploy(deployId: String): F[LightBlockInfo]

  def exploratoryDeploy(
      term: String,
      blockHash: Option[String],
      usePreStateHash: Boolean
  ): F[ExploratoryDeployResponse]

  def getBlocksByHeights(startBlockNumber: Long, endBlockNumber: Long): F[List[LightBlockInfo]]

  def isFinalized(hash: String): F[Boolean]
}

object WebApi {

  class WebApiImpl[F[_]: Sync: Concurrent: EngineCell: Log: Span: BlockStore](
      apiMaxBlocksLimit: Int,
      devMode: Boolean = false,
      triggerProposeF: Option[ProposeFunction[F]]
  ) extends WebApi[F] {
    import WebApiSyntax._

    def prepareDeploy(req: Option[PrepareRequest]): F[PrepareResponse] = {
      val seqNumber = BlockAPI
        .getLatestMessage[F]
        .flatMap(_.liftToBlockApiErr)
        .map(_.seqNum)
        .handleError { case _: LatestBlockMessageError => -1 }

      val previewNames = req.fold(List[String]().pure) { r =>
        BlockAPI
          .previewPrivateNames[F](toByteString(r.deployer), r.timestamp, r.nameQty)
          .flatMap(_.liftToBlockApiErr)
          .map(_.map(toHex).toList)
      }

      (previewNames, seqNumber).mapN(PrepareResponse)
    }

    def deploy(request: DeployRequest): F[String] =
      toSignedDeploy(request)
        .flatMap(BlockAPI.deploy(_, triggerProposeF))
        .flatMap(_.liftToBlockApiErr)

    def listenForDataAtName(req: DataRequest): F[DataResponse] =
      BlockAPI
        .getListeningNameDataResponse(req.depth, toPar(req), apiMaxBlocksLimit)
        .flatMap(_.liftToBlockApiErr)
        .map(toDataResponse)

    def lastFinalizedBlock: F[BlockInfo] =
      BlockAPI.lastFinalizedBlock[F].flatMap(_.liftToBlockApiErr)

    def getBlock(hash: String): F[BlockInfo] =
      BlockAPI.getBlock[F](hash).flatMap(_.liftToBlockApiErr)

    def getBlocks(depth: Int): F[List[LightBlockInfo]] =
      BlockAPI.getBlocks[F](depth, apiMaxBlocksLimit).flatMap(_.liftToBlockApiErr)

    def findDeploy(deployId: String): F[LightBlockInfo] =
      BlockAPI
        .findDeploy[F](toByteString(deployId))
        .flatMap(_.liftToBlockApiErr)

    def exploratoryDeploy(
        term: String,
        blockHash: Option[String],
        usePreStateHash: Boolean
    ): F[ExploratoryDeployResponse] =
      BlockAPI
        .exploratoryDeploy(term, blockHash, usePreStateHash, devMode)
        .flatMap(_.liftToBlockApiErr)
        .map(toExploratoryResponse)

    def status: F[ApiStatus] =
      ApiStatus(
        version = 1,
        message = "OK"
      ).pure

    def getBlocksByHeights(startBlockNumber: Long, endBlockNumber: Long): F[List[LightBlockInfo]] =
      BlockAPI
        .getBlocksByHeights(startBlockNumber, endBlockNumber, apiMaxBlocksLimit)
        .flatMap(_.liftToBlockApiErr)

    def isFinalized(hash: String): F[Boolean] =
      BlockAPI.isFinalized(hash).flatMap(_.liftToBlockApiErr)
  }

  // Rholang terms interesting for translation to JSON

  sealed trait RhoExpr
  // Nested expressions (Par, Tuple, List and Set are converted to JSON list)
  final case class ExprPar(data: List[RhoExpr])        extends RhoExpr
  final case class ExprTuple(data: List[RhoExpr])      extends RhoExpr
  final case class ExprList(data: List[RhoExpr])       extends RhoExpr
  final case class ExprSet(data: List[RhoExpr])        extends RhoExpr
  final case class ExprMap(data: Map[String, RhoExpr]) extends RhoExpr
  // Terminal expressions (here is the data)
  final case class ExprBool(data: Boolean)  extends RhoExpr
  final case class ExprInt(data: Long)      extends RhoExpr
  final case class ExprString(data: String) extends RhoExpr
  final case class ExprUri(data: String)    extends RhoExpr
  // Binary data is encoded as base16 string
  final case class ExprBytes(data: String)     extends RhoExpr
  final case class ExprUnforg(data: RhoUnforg) extends RhoExpr

  // Unforgeable name
  sealed trait RhoUnforg
  final case class UnforgPrivate(data: String)  extends RhoUnforg
  final case class UnforgDeploy(data: String)   extends RhoUnforg
  final case class UnforgDeployer(data: String) extends RhoUnforg

  // API request & response types

  final case class DeployRequest(
      data: DeployData,
      deployer: String,
      signature: String,
      sigAlgorithm: String
  )

  final case class ExploreDeployRequest(
      term: String,
      blockHash: String,
      usePreStateHash: Boolean
  )

  final case class DataRequest(
      // For simplicity only one Unforgeable name is allowed
      // instead of the whole RhoExpr (proto Par)
      name: RhoUnforg,
      // Number of blocks in the past to search for data
      depth: Int
  )

  final case class DataResponse(
      exprs: List[RhoExprWithBlock],
      length: Int
  )

  final case class RhoExprWithBlock(
      expr: RhoExpr,
      block: LightBlockInfo
  )

  final case class ExploratoryDeployResponse(
      expr: Seq[RhoExpr],
      block: LightBlockInfo
  )

  final case class PrepareRequest(
      deployer: String,
      timestamp: Long,
      nameQty: Int
  )

  final case class PrepareResponse(
      names: List[String],
      seqNumber: Int
  )

  final case class ApiStatus(
      version: Int,
      message: String
  )

  // Exception thrown by BlockAPI
  final class BlockApiException(message: String) extends Exception(message)

  // Deploy signature error
  final class SignatureException(message: String) extends Exception(message)

  // Conversion functions for protobuf generated types

  import WebApiSyntax._

  def toSignedDeploy[F[_]: Sync](
      sd: DeployRequest
  ): F[Signed[DeployData]] =
    for {
      pkBytes <- Base16
                  .decode(sd.deployer)
                  .liftToSigErr[F]("Public key is not valid base16 format.")
      sigBytes <- Base16
                   .decode(sd.signature)
                   .liftToSigErr[F]("Signature is not valid base16 format.")
      sig    = ByteString.copyFrom(sigBytes)
      pk     = PublicKey(pkBytes)
      sigAlg <- SignaturesAlg(sd.sigAlgorithm).liftToSigErr[F]("Signature algorithm not supported.")
      sigData <- Signed
                  .fromSignedData(sd.data, pk, sig, sigAlg)
                  .liftToSigErr[F]("Invalid signature.")
    } yield sigData

  // Binary converters - protobuf uses ByteString and in JSON is base16 string

  private def toHex(bs: ByteString) = Base16.encode(bs.toByteArray)

  private def toByteString(hexStr: String) = ByteString.copyFrom(Base16.unsafeDecode(hexStr))

  // RhoExpr from protobuf

  private def exprFromParProto(par: Par): Option[RhoExpr] = {
    val exprs = par.exprs.flatMap(exprFromExprProto) ++ par.unforgeables.flatMap(unforgFromProto)
    // Implements semantic of Par with Unit: P | Nil ==> P
    if (exprs.size == 1) exprs.head.some
    else if (exprs.isEmpty) none
    else ExprPar(exprs.toList).some
  }

  private def exprFromExprProto(exp: Expr): Option[RhoExpr] =
    // Primitive types
    if (exp.exprInstance.isGBool)
      ExprBool(exp.getGBool).some
    else if (exp.exprInstance.isGInt)
      ExprInt(exp.getGInt).some
    else if (exp.exprInstance.isGString)
      ExprString(exp.getGString).some
    else if (exp.exprInstance.isGUri)
      ExprUri(exp.getGUri).some
    else if (exp.exprInstance.isGByteArray)
      // Binary data as base16 string
      ExprBytes(toHex(exp.getGByteArray)).some
    // Tuple
    else if (exp.exprInstance.isETupleBody)
      ExprTuple(exp.getETupleBody.ps.flatMap(exprFromParProto).toList).some
    // List
    else if (exp.exprInstance.isEListBody)
      ExprList(exp.getEListBody.ps.flatMap(exprFromParProto).toList).some
    // Set
    else if (exp.exprInstance.isESetBody)
      ExprSet(exp.getESetBody.ps.flatMap(exprFromParProto).toList).some
    // Map
    else if (exp.exprInstance.isEMapBody) {
      val fields = for {
        (k, v)  <- exp.getEMapBody.ps
        keyExpr <- exprFromParProto(k)
        key <- keyExpr match {
                case ExprString(str) => str.some
                case ExprInt(num)    => num.toString.some
                case ExprBool(bool)  => bool.toString.some
                case ExprUri(uri)    => uri.some
                case ExprUnforg(unforg) =>
                  unforg match {
                    case UnforgPrivate(hex)  => hex.some
                    case UnforgDeploy(hex)   => hex.some
                    case UnforgDeployer(hex) => hex.some
                  }
                case ExprBytes(hex) => hex.some
                case _              => none
              }
        value <- exprFromParProto(v)
      } yield (key, value)
      ExprMap(fields.toMap).some
    } else none

  private def unforgFromProto(un: GUnforgeable): Option[ExprUnforg] =
    if (un.unfInstance.isGPrivateBody)
      mkUnforgExpr(UnforgPrivate, un.unfInstance.gPrivateBody.get.id).some
    else if (un.unfInstance.isGDeployIdBody)
      mkUnforgExpr(UnforgDeploy, un.unfInstance.gDeployIdBody.get.sig).some
    else if (un.unfInstance.isGDeployerIdBody)
      mkUnforgExpr(UnforgDeployer, un.unfInstance.gDeployerIdBody.get.publicKey).some
    else none

  private def mkUnforgExpr(f: String => RhoUnforg, bs: ByteString): ExprUnforg =
    ExprUnforg(f(toHex(bs)))

  // RhoExpr to protobuf

  private def unforgToUnforgProto(unforg: RhoUnforg): GUnforgeable.UnfInstance = unforg match {
    case UnforgPrivate(name)  => GPrivateBody(GPrivate(toByteString(name)))
    case UnforgDeploy(name)   => GDeployIdBody(GDeployId(toByteString(name)))
    case UnforgDeployer(name) => GDeployerIdBody(GDeployerId(toByteString(name)))
  }

  // Data request/response protobuf wrappers

  private def toPar(req: DataRequest): Par =
    Par(unforgeables = Seq(GUnforgeable(unforgToUnforgProto(req.name))))

  private def toDataResponse(req: (Seq[DataWithBlockInfo], Int)): DataResponse = {
    val (dbs, length) = req
    val exprsWithBlock = dbs.foldLeft(List[RhoExprWithBlock]()) { (acc, data) =>
      val exprs = data.postBlockData.flatMap(exprFromParProto)
      // Implements semantic of Par with Unit: P | Nil ==> P
      val expr  = if (exprs.size == 1) exprs.head else ExprPar(exprs.toList)
      val block = data.block.get
      RhoExprWithBlock(expr, block) +: acc
    }
    DataResponse(exprsWithBlock, length)
  }

  private def toExploratoryResponse(data: (Seq[Par], LightBlockInfo)) = {
    val (pars, lightBlockInfo) = data
    val rhoExprs               = pars.flatMap(exprFromParProto)
    ExploratoryDeployResponse(rhoExprs, lightBlockInfo)
  }

}
