package coop.rchain.casper.api

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.api.BlockApi.ApiErr
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.{BlockMetadata, Par}

trait BlockApi[F[_]] {
  def status: F[Status]

  def deploy(d: Signed[DeployData]): F[ApiErr[String]]

  def createBlock(isAsync: Boolean): F[ApiErr[String]]

  def getProposeResult: F[ApiErr[String]]

  def getListeningNameDataResponse(
      depth: Int,
      listeningName: Par
  ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]]

  def getListeningNameContinuationResponse(
      depth: Int,
      listeningNames: Seq[Par]
  ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]]

  def getBlocksByHeights(
      startBlockNumber: Long,
      endBlockNumber: Long
  ): F[ApiErr[List[LightBlockInfo]]]

  def visualizeDag[R](
      depth: Int,
      startBlockNumber: Int,
      showJustificationLines: Boolean
  ): F[ApiErr[Vector[String]]]

  def machineVerifiableDag(depth: Int): F[ApiErr[String]]

  def getBlocks(depth: Int): F[ApiErr[List[LightBlockInfo]]]

  def findDeploy(id: DeployId): F[ApiErr[LightBlockInfo]]

  def getBlock(hash: String): F[ApiErr[BlockInfo]]

  def previewPrivateNames(
      deployer: ByteString,
      timestamp: Long,
      nameQty: Int
  ): F[ApiErr[Seq[ByteString]]]

  def bondStatus(
      publicKey: ByteString,
      targetBlock: Option[BlockMessage] = none[BlockMessage]
  ): F[ApiErr[Boolean]]

  def exploratoryDeploy(
      term: String,
      blockHash: Option[String],
      usePreStateHash: Boolean
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]]

  def getDataAtPar(
      par: Par,
      blockHash: String,
      usePreStateHash: Boolean
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]]

  def lastFinalizedBlock: F[ApiErr[BlockInfo]]

  def isFinalized(hash: String): F[ApiErr[Boolean]]

  def getLatestMessage: F[ApiErr[BlockMetadata]]
}

object BlockApi {
  type Error     = String
  type ApiErr[A] = Either[Error, A]

  private def getBlockInfo[A](
      block: BlockMessage,
      constructor: (BlockMessage, Float) => A
  ): A = constructor(block, -1f)

  def getFullBlockInfo(block: BlockMessage): BlockInfo =
    getBlockInfo[BlockInfo](block, constructBlockInfo)

  def getLightBlockInfo(block: BlockMessage): LightBlockInfo =
    getBlockInfo[LightBlockInfo](block, constructLightBlockInfo)

  private def constructBlockInfo(
      block: BlockMessage,
      faultTolerance: Float
  ): BlockInfo = {
    val lightBlockInfo = constructLightBlockInfo(block, faultTolerance)
    val deploys        = block.body.deploys.map(_.toDeployInfo)
    BlockInfo(blockInfo = lightBlockInfo, deploys = deploys)
  }

  private def constructLightBlockInfo(
      block: BlockMessage,
      faultTolerance: Float
  ): LightBlockInfo =
    LightBlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      sender = PrettyPrinter.buildStringNoLimit(block.sender),
      seqNum = block.seqNum.toLong,
      sig = PrettyPrinter.buildStringNoLimit(block.sig),
      sigAlgorithm = block.sigAlgorithm,
      shardId = block.shardId,
      extraBytes = block.extraBytes,
      version = block.header.version,
      timestamp = block.header.timestamp,
      headerExtraBytes = block.header.extraBytes,
      parentsHashList = block.header.parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      blockNumber = block.body.state.blockNumber,
      preStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.preStateHash),
      postStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.postStateHash),
      bodyExtraBytes = block.body.extraBytes,
      bonds = block.body.state.bonds.map(ProtoUtil.bondToBondInfo),
      blockSize = block.toProto.serializedSize.toString,
      deployCount = block.body.deploys.length,
      faultTolerance = faultTolerance,
      justifications = block.justifications.map(ProtoUtil.justificationsToJustificationInfos),
      rejectedDeploys = block.body.rejectedDeploys.map(
        r => RejectedDeployInfo(PrettyPrinter.buildStringNoLimit(r.sig))
      )
    )
}
