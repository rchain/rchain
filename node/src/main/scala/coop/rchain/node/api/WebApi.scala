package coop.rchain.node.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.{
  BondInfo,
  DataWithBlockInfo,
  DeployData,
  DeployInfo,
  BlockInfo => BlockInfoProto,
  LightBlockInfo => LightBlockInfoProto
}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{SignaturesAlg, Signed}
import coop.rchain.metrics.Span
import coop.rchain.models.GUnforgeable.UnfInstance.{GDeployIdBody, GDeployerIdBody, GPrivateBody}
import coop.rchain.models._
import coop.rchain.node.api.WebApi._
import coop.rchain.shared.Log

trait WebApi[F[_]] {
  def status: F[ApiStatus]

  // Get data to create deploy
  def prepareDeploy(request: Option[PrepareRequest]): F[PrepareResponse]

  // Write data (deploy)
  def deploy(request: Signed[DeployData]): F[String]

  // Read data (listen)
  def listenForDataAtName(request: DataRequest): F[DataResponse]

  // Blocks info

  def lastFinalizedBlock: F[BlockInfo]

  def getBlock(hash: String): F[BlockInfo]

  def getBlocks(depth: Option[Int]): F[List[LightBlockInfo]]

  def findDeploy(deployId: String): F[LightBlockInfo]
}

object WebApi {

  class WebApiImpl[F[_]: Sync: Concurrent: EngineCell: Log: Span: SafetyOracle: BlockStore]
      extends WebApi[F] {
    import WebApiSyntax._

    def prepareDeploy(req: Option[PrepareRequest]): F[PrepareResponse] = {
      val blockNumber = BlockAPI
        .getBlocks[F](1.some)
        .flatMap(_.liftToBlockApiErr)
        .map(_.headOption.map(_.blockNumber).getOrElse(-1L))

      val previewNames = req.fold(List[String]().pure) { r =>
        BlockAPI
          .previewPrivateNames[F](toByteString(r.deployer), r.timestamp, r.nameQty)
          .flatMap(_.liftToBlockApiErr)
          .map(_.map(toHex).toList)
      }

      previewNames.map2(blockNumber)(PrepareResponse)
    }

    def deploy(request: Signed[DeployData]): F[String] =
      BlockAPI.deploy(request).flatMap(_.liftToBlockApiErr)

    def listenForDataAtName(req: DataRequest): F[DataResponse] =
      BlockAPI
        .getListeningNameDataResponse(req.depth, toPar(req))
        .flatMap(_.liftToBlockApiErr)
        .map(toDataResponse)

    def lastFinalizedBlock: F[BlockInfo] =
      BlockAPI.lastFinalizedBlock[F].flatMap(_.liftToBlockApiErr).map(toBlockInfo)

    def getBlock(hash: String): F[BlockInfo] =
      BlockAPI.getBlock[F](hash).flatMap(_.liftToBlockApiErr).map(toBlockInfo)

    def getBlocks(depth: Option[Int]): F[List[LightBlockInfo]] =
      BlockAPI.getBlocks[F](depth).flatMap(_.liftToBlockApiErr).map(_.map(toLightBlockInfo))

    def findDeploy(deployId: String): F[LightBlockInfo] =
      BlockAPI
        .findDeploy[F](toByteString(deployId))
        .flatMap(_.liftToBlockApiErr)
        .map(toLightBlockInfo)

    def status: F[ApiStatus] =
      ApiStatus(
        version = 1,
        message = "OK"
      ).pure
  }

  // API request & response types

  final case class SignedDeploy(
      data: DeployData,
      deployer: String,
      signature: String,
      sigAlgorithm: String
  )

  // Rholang terms interesting for translation to JSON

  sealed trait RhoExpr
  // Nested expressions
  final case class ExprPar(data: List[RhoExpr])        extends RhoExpr
  final case class ExprList(data: List[RhoExpr])       extends RhoExpr
  final case class ExprTuple(data: List[RhoExpr])      extends RhoExpr
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

  final case class LightBlockInfo(
      blockHash: String,
      sender: String,
      seqNum: Long,
      sig: String,
      sigAlgorithm: String,
      shardId: String,
      extraBytes: String,
      version: Long,
      timestamp: Long,
      headerExtraBytes: String,
      parentsHashList: List[String],
      blockNumber: Long,
      preStateHash: String,
      postStateHash: String,
      bodyExtraBytes: String,
      bonds: List[BondInfo],
      blockSize: String,
      deployCount: Int,
      faultTolerance: Float
  )

  final case class BlockInfo(
      blockInfo: LightBlockInfo,
      deploys: List[DeployInfo]
  )

  final case class PrepareRequest(
      deployer: String,
      timestamp: Long,
      nameQty: Int
  )

  final case class PrepareResponse(
      names: List[String],
      blockNumber: Long
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

  def toSigned[F[_]: Sync](
      sd: SignedDeploy
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
    // Map
    else if (exp.exprInstance.isEMapBody) {
      val fields = for {
        (k, v) <- exp.getEMapBody.ps
        expr   <- k.exprs.headOption
        // Only String keys are accepted
        ExprString(key) <- exprFromExprProto(expr)
        value           <- exprFromParProto(v)
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
      val block = toLightBlockInfo(data.block.get)
      RhoExprWithBlock(expr, block) +: acc
    }
    DataResponse(exprsWithBlock, length)
  }

  private def toLightBlockInfo(b: LightBlockInfoProto): LightBlockInfo =
    LightBlockInfo(
      blockHash = b.blockHash,
      sender = b.sender,
      seqNum = b.seqNum,
      sig = b.sig,
      sigAlgorithm = b.sigAlgorithm,
      shardId = b.shardId,
      extraBytes = toHex(b.extraBytes),
      version = b.version,
      timestamp = b.timestamp,
      headerExtraBytes = toHex(b.headerExtraBytes),
      parentsHashList = b.parentsHashList.toList,
      blockNumber = b.blockNumber,
      preStateHash = b.preStateHash,
      postStateHash = b.postStateHash,
      bodyExtraBytes = toHex(b.bodyExtraBytes),
      bonds = b.bonds.toList,
      blockSize = b.blockSize,
      deployCount = b.deployCount,
      faultTolerance = b.faultTolerance
    )

  private def toBlockInfo(b: BlockInfoProto): BlockInfo =
    BlockInfo(
      blockInfo = toLightBlockInfo(b.blockInfo.get),
      deploys = b.deploys.toList
    )

  object WebApiSyntax {
    implicit final class OptionExt[A](val x: Option[A]) extends AnyVal {
      def liftToSigErr[F[_]: Sync](error: String): F[A] =
        x.liftTo[F](new SignatureException(error))
    }

    implicit final class EitherStringExt[A](val x: Either[String, A]) extends AnyVal {
      def liftToBlockApiErr[F[_]: Sync]: F[A] =
        x.leftMap(new BlockApiException(_)).liftTo[F]
    }
  }

}
