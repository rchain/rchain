package coop.rchain.api

import cats.effect.concurrent.Ref
import cats.implicits.none
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.ProposeFunction
import coop.rchain.casper.protocol._
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.crypto.signatures.Signed
import coop.rchain.graphz.Graphz
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockMetadata, Par}

trait BlockAPI_v2[F[_]] {
  type Error     = String
  type ApiErr[A] = Either[Error, A]

  def deploy(
      d: Signed[DeployData],
      triggerPropose: Option[ProposeFunction[F]],
      minPhloPrice: Long,
      isNodeReadOnly: Boolean,
      shardId: String
  ): F[ApiErr[String]]

  def createBlock(triggerProposeF: ProposeFunction[F], isAsync: Boolean = false): F[ApiErr[String]]

  def getProposeResult(proposerState: Ref[F, ProposerState[F]]): F[ApiErr[String]]

  def getListeningNameDataResponse(
      depth: Int,
      listeningName: Par,
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]]

  def getListeningNameContinuationResponse(
      depth: Int,
      listeningNames: Seq[Par],
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]]

  def getBlocksByHeights(
      startBlockNumber: Long,
      endBlockNumber: Long,
      maxBlocksLimit: Int
  ): F[ApiErr[List[LightBlockInfo]]]

  def visualizeDag[R](
      depth: Int,
      maxDepthLimit: Int,
      startBlockNumber: Int,
      visualizer: (Vector[Vector[BlockHash]], String) => F[Graphz[F]],
      serialize: F[R]
  ): F[ApiErr[R]]

  def machineVerifiableDag(depth: Int, maxDepthLimit: Int): F[ApiErr[String]]

  def getBlocks(depth: Int, maxDepthLimit: Int): F[ApiErr[List[LightBlockInfo]]]

  def showMainChain(depth: Int, maxDepthLimit: Int): F[List[LightBlockInfo]]

  def findDeploy(id: DeployId): F[ApiErr[LightBlockInfo]]

  def getBlock(hash: String): F[ApiErr[BlockInfo]]

  def getLightBlockInfo(block: BlockMessage): F[LightBlockInfo]

  def previewPrivateNames(
      deployer: ByteString,
      timestamp: Long,
      nameQty: Int
  ): F[ApiErr[Seq[ByteString]]]

  def lastFinalizedBlock: F[ApiErr[BlockInfo]]

  def isFinalized(hash: String): F[ApiErr[Boolean]]

  def bondStatus(publicKey: ByteString): F[ApiErr[Boolean]]

  def exploratoryDeploy(
      term: String,
      blockHash: Option[String] = none,
      usePreStateHash: Boolean = false,
      devMode: Boolean = false
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]]

  def getLatestMessage: F[ApiErr[BlockMetadata]]

  def getDataAtPar(
      par: Par,
      blockHash: String,
      usePreStateHash: Boolean
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]]
}
