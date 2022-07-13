package coop.rchain.node.api

import cats.data.OptionT
import cats.effect.Sync
import cats.kernel.Monoid
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.api.BlockApi
import coop.rchain.casper.protocol.{BlockInfo, DataWithBlockInfo, DeployData, LightBlockInfo}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.{SignaturesAlg, Signed}
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.syntax._
import coop.rchain.models.{Bundle, ETuple, Expr, GUnforgeable, Par}
import coop.rchain.node.api.WebApi._
import coop.rchain.node.web.{CacheTransactionAPI, TransactionResponse}
import coop.rchain.rholang.interpreter.RhoType._

trait WebApi[F[_]] {
  def status: F[ApiStatus]

  // Write data (deploy)
  def deploy(request: DeployRequest): F[String]

  def deployStatus(deployId: String): F[DeployExecStatus]

  // Read data (listen)
  def listenForDataAtName(request: DataAtNameRequest): F[DataAtNameResponse]

  def getDataAtPar(request: DataAtNameByBlockHashRequest): F[RhoDataResponse]

  // Blocks info

  def lastFinalizedBlock: F[BlockInfo]

  def getBlock(hash: String): F[BlockInfo]

  def getBlocks(depth: Int): F[List[LightBlockInfo]]

  def findDeploy(deployId: String): F[LightBlockInfo]

  def exploratoryDeploy(
      term: String,
      blockHash: Option[String],
      usePreStateHash: Boolean
  ): F[RhoDataResponse]

  def getBlocksByHeights(startBlockNumber: Long, endBlockNumber: Long): F[List[LightBlockInfo]]

  def isFinalized(hash: String): F[Boolean]

  def getTransaction(hash: String): F[TransactionResponse]
}

object WebApi {

  class WebApiImpl[F[_]: Sync](
      blockApi: BlockApi[F],
      cacheTransactionAPI: CacheTransactionAPI[F]
  ) extends WebApi[F] {
    import WebApiSyntax._

    def status: F[ApiStatus] = blockApi.status.map(toApiStatus)

    def deploy(request: DeployRequest): F[String] =
      toSignedDeploy(request)
        .flatMap(blockApi.deploy)
        .flatMap(_.liftToBlockApiErr)

    def deployStatus(deploySignature: String): F[DeployExecStatus] =
      blockApi
        .deployStatus(deploySignature.unsafeHexToByteString)
        .flatMap(_.liftToBlockApiErr)
        .flatMap(
          toDeployExecStatus(_).liftTo(new Exception("Deploy status protobuf message error"))
        )

    def listenForDataAtName(req: DataAtNameRequest): F[DataAtNameResponse] =
      blockApi
        .getListeningNameDataResponse(req.depth, toPar(req))
        .flatMap(_.liftToBlockApiErr)
        .map(toDataAtNameResponse)

    def getDataAtPar(req: DataAtNameByBlockHashRequest): F[RhoDataResponse] =
      blockApi
        .getDataAtPar(toPar(req), req.blockHash, req.usePreStateHash)
        .flatMap(_.liftToBlockApiErr)
        .map(toRhoDataResponse)

    def lastFinalizedBlock: F[BlockInfo] =
      blockApi.lastFinalizedBlock.flatMap(_.liftToBlockApiErr)

    def getBlock(hash: String): F[BlockInfo] =
      blockApi.getBlock(hash).flatMap(_.liftToBlockApiErr)

    def getBlocks(depth: Int): F[List[LightBlockInfo]] =
      blockApi.getBlocks(depth).flatMap(_.liftToBlockApiErr)

    def findDeploy(deployId: String): F[LightBlockInfo] =
      blockApi
        .findDeploy(deployId.unsafeHexToByteString)
        .flatMap(_.liftToBlockApiErr)

    def exploratoryDeploy(
        term: String,
        blockHash: Option[String],
        usePreStateHash: Boolean
    ): F[RhoDataResponse] =
      blockApi
        .exploratoryDeploy(term, blockHash, usePreStateHash)
        .flatMap(_.liftToBlockApiErr)
        .map(toRhoDataResponse)

    def getBlocksByHeights(startBlockNumber: Long, endBlockNumber: Long): F[List[LightBlockInfo]] =
      blockApi
        .getBlocksByHeights(startBlockNumber, endBlockNumber)
        .flatMap(_.liftToBlockApiErr)

    def isFinalized(hash: String): F[Boolean] =
      blockApi.isFinalized(hash).flatMap(_.liftToBlockApiErr)

    def getTransaction(hash: String): F[TransactionResponse] =
      OptionT
        .whenF(hash.nonEmpty)(cacheTransactionAPI.getTransaction(hash))
        .value
        .flatMap(_.liftTo(new Exception("Block hash cannot be empty.")))
  }

  // Rholang terms interesting for translation to JSON

  sealed trait RhoExpr
  // Nested expressions (Par, Tuple, List and Set are converted to JSON list)
  final case class ExprPar(data: List[RhoExpr])        extends RhoExpr
  final case class ExprTuple(data: List[RhoExpr])      extends RhoExpr
  final case class ExprList(data: List[RhoExpr])       extends RhoExpr
  final case class ExprSet(data: Set[RhoExpr])         extends RhoExpr
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

  final case class DataAtNameRequest(
      // For simplicity only one Unforgeable name is allowed
      // instead of the whole RhoExpr (proto Par)
      name: RhoUnforg,
      // Number of blocks in the past to search for data
      depth: Int
  )

  final case class DataAtNameByBlockHashRequest(
      name: RhoExpr,
      blockHash: String,
      usePreStateHash: Boolean
  )

  final case class DataAtNameResponse(
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

  sealed trait DeployExecStatus

  final case class ProcessedWithSuccess(
      deployResult: Seq[RhoExpr],
      block: LightBlockInfo
  ) extends DeployExecStatus

  final case class ProcessedWithError(
      deployError: String,
      block: LightBlockInfo
  ) extends DeployExecStatus

  final case class NotProcessed(status: String) extends DeployExecStatus

  final case class RhoDataResponse(
      expr: Seq[RhoExpr],
      block: LightBlockInfo
  )

  final case class ApiStatus(
      version: VersionInfo,
      address: String,
      networkId: String,
      shardId: String,
      peers: Int,
      nodes: Int,
      minPhloPrice: Long,
      latestBlockNumber: Long
  )

  final case class VersionInfo(api: String, node: String)

  // Exception thrown by BlockAPI
  final class BlockApiException(message: String) extends Exception(message)

  // Deploy signature error
  final class SignatureException(message: String) extends Exception(message)

  // Conversion functions for protobuf generated types

  import WebApiSyntax._

  def toApiStatus(status: coop.rchain.casper.protocol.Status) =
    ApiStatus(
      version = VersionInfo(api = status.version.api, node = status.version.node),
      address = status.address,
      networkId = status.networkId,
      shardId = status.shardId,
      peers = status.peers,
      nodes = status.nodes,
      minPhloPrice = status.minPhloPrice,
      latestBlockNumber = status.latestBlockNumber
    )

  def toSignedDeploy[F[_]: Sync](
      sd: DeployRequest
  ): F[Signed[DeployData]] =
    for {
      pkBytes <- sd.deployer.decodeHex
                  .liftToSigErr[F]("Public key is not valid base16 format.")
      sigBytes <- sd.signature.decodeHex
                   .liftToSigErr[F]("Signature is not valid base16 format.")
      sig    = ByteString.copyFrom(sigBytes)
      pk     = PublicKey(pkBytes)
      sigAlg <- SignaturesAlg(sd.sigAlgorithm).liftToSigErr[F]("Signature algorithm not supported.")
      sigData <- Signed
                  .fromSignedData(sd.data, pk, sig, sigAlg)
                  .liftToSigErr[F]("Invalid signature.")
    } yield sigData

  // RhoExpr from protobuf

  def exprFromParProto(par: Par): Option[RhoExpr] = {
    val exprs =
      par.exprs.flatMap(exprFromExprProto) ++
        par.unforgeables.flatMap(unforgFromProto) ++
        par.bundles.flatMap(exprFromBundleProto)
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
      ExprBytes(exp.getGByteArray.toHexString).some
    // Tuple
    else if (exp.exprInstance.isETupleBody)
      ExprTuple(exp.getETupleBody.ps.flatMap(exprFromParProto).toList).some
    // List
    else if (exp.exprInstance.isEListBody)
      ExprList(exp.getEListBody.ps.flatMap(exprFromParProto).toList).some
    // Set
    else if (exp.exprInstance.isESetBody)
      ExprSet(exp.getESetBody.ps.flatMap(exprFromParProto).toSet).some
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

  private def exprFromBundleProto(b: Bundle): Option[RhoExpr] = exprFromParProto(b.body)

  private def mkUnforgExpr(f: String => RhoUnforg, bs: ByteString): ExprUnforg =
    ExprUnforg(f(bs.toHexString))

  // RhoExpr to protobuf

  /**
    * Private definition of [[Monoid]] for [[Par]] with the implementation of
    * combining fields used by WebApi representation of Par [[RhoExpr]].
    */
  implicit private object ParMonoid extends Monoid[Par] {
    override def empty: Par = VectorPar()

    override def combine(x: Par, y: Par): Par =
      x.copy(
        x.sends ++ y.sends,
        x.receives ++ y.receives,
        x.news ++ y.news,
        x.exprs ++ y.exprs,
        x.matches ++ y.matches,
        x.unforgeables ++ y.unforgeables,
        x.bundles ++ y.bundles,
        x.connectives ++ y.connectives
      )
  }

  /**
    * Converts [[RhoExpr]] WebApi representation to protobuf [[Par]] type.
    */
  def rhoExprToParProto(exp: RhoExpr): Par = exp match {
    // Nested expressions (Par, Tuple, List and Set are converted to JSON list)
    case ExprPar(data)   => data.map(rhoExprToParProto).combineAll
    case ExprTuple(data) => TupleN(data.map(rhoExprToParProto))
    case ExprList(data)  => List(data.map(rhoExprToParProto))
    case ExprSet(data)   => Set(data.map(rhoExprToParProto).toSeq)
    case ExprMap(data)   => Map(data.map { case (k, v) => (String(k), rhoExprToParProto(v)) })
    // Terminal expressions (here is the data)
    case ExprBool(data)   => Boolean(data)
    case ExprInt(data)    => Number(data)
    case ExprString(data) => String(data)
    case ExprUri(data)    => Uri(data)
    // Binary data is decoded from base16 string
    case ExprBytes(data)  => ByteArray(data.unsafeHexToByteString.toByteArray)
    case ExprUnforg(data) => unforgToParProto(data)
  }

  private def unforgToParProto(unforg: RhoUnforg): Par = unforg match {
    case UnforgPrivate(name)  => Name(name.unsafeHexToByteString)
    case UnforgDeploy(name)   => DeployId(name.unsafeDecodeHex)
    case UnforgDeployer(name) => DeployerId(name.unsafeDecodeHex)
  }

  // Data request/response protobuf wrappers

  private def toPar(req: DataAtNameRequest): Par = unforgToParProto(req.name)

  private def toPar(req: DataAtNameByBlockHashRequest): Par = rhoExprToParProto(req.name)

  private def toDataAtNameResponse(req: (Seq[DataWithBlockInfo], Int)): DataAtNameResponse = {
    val (dbs, length) = req
    val exprsWithBlock = dbs.foldLeft(collection.immutable.List[RhoExprWithBlock]()) {
      (acc, data) =>
        val exprs = data.postBlockData.flatMap(exprFromParProto)
        // Implements semantic of Par with Unit: P | Nil ==> P
        val expr  = if (exprs.size == 1) exprs.head else ExprPar(exprs.toList)
        val block = data.block
        RhoExprWithBlock(expr, block) +: acc
    }
    DataAtNameResponse(exprsWithBlock, length)
  }

  private def toDeployExecStatus(
      status: coop.rchain.casper.protocol.deploy.v1.DeployExecStatus
  ): Option[DeployExecStatus] = {
    import coop.rchain.casper.protocol.deploy.v1.DeployExecStatus.{Status => DepSt}
    status.status match {
      case DepSt.ProcessedWithSuccess(s) =>
        ProcessedWithSuccess(
          s.deployResult.map(exprFromParProto(_).get),
          s.block
        ).some
      case DepSt.ProcessedWithError(s) =>
        ProcessedWithError(s.deployError, s.block).some
      case DepSt.NotProcessed(s) => NotProcessed(s.status).some
      case DepSt.Empty           => none
    }
  }

  private def toRhoDataResponse(data: (Seq[Par], LightBlockInfo)): RhoDataResponse = {
    val (pars, lightBlockInfo) = data
    val rhoExprs               = pars.flatMap(exprFromParProto)
    RhoDataResponse(rhoExprs, lightBlockInfo)
  }
}
