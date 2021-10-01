package coop.rchain.casper

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Show}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState.BlockDagFinalizationState
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.merging.DeployChainIndex
import coop.rchain.casper.protocol._
import coop.rchain.casper.state.NetworkStateMerged
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.v2.core.Casper.{
  ConflictScope,
  FinalizationFringe,
  LatestMessages,
  MessageScope
}
import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared._

sealed trait DeployError
final case class ParsingError(details: String)          extends DeployError
final case object MissingUser                           extends DeployError
final case class UnknownSignatureAlgorithm(alg: String) extends DeployError
final case object SignatureVerificationFailed           extends DeployError

object DeployError {
  def parsingError(details: String): DeployError          = ParsingError(details)
  def missingUser: DeployError                            = MissingUser
  def unknownSignatureAlgorithm(alg: String): DeployError = UnknownSignatureAlgorithm(alg)
  def signatureVerificationFailed: DeployError            = SignatureVerificationFailed

  implicit val showDeployError: Show[DeployError] = new Show[DeployError] {
    def show(error: DeployError): String = error match {
      case ParsingError(details)          => s"Parsing error: $details"
      case MissingUser                    => s"Missing user"
      case UnknownSignatureAlgorithm(alg) => s"Unknown signature algorithm '$alg'"
      case SignatureVerificationFailed    => "Signature verification failed"
    }
  }
}

trait Casper[F[_]] {
  def getSnapshot(targetBlockOpt: Option[BlockMessage] = None): F[CasperSnapshot[F]]
  def dagContains(hash: BlockHash): F[Boolean]
  def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]]
  def getValidator: F[Option[ValidatorIdentity]]
  def getVersion: F[Long]

  def validate(
      b: BlockMessage,
      s: CasperSnapshot[F]
  ): F[Either[BlockError, ValidBlock]]
  def handleValidBlock(block: BlockMessage, s: CasperSnapshot[F]): F[BlockDagRepresentation[F]]
  def handleInvalidBlock(
      block: BlockMessage,
      status: InvalidBlock,
      s: CasperSnapshot[F]
  ): F[BlockDagRepresentation[F]]
}

trait MultiParentCasper[F[_]] extends Casper[F] {
  def blockDag: F[BlockDagRepresentation[F]]
  // This is the weight of faults that have been accumulated so far.
  // We want the clique oracle to give us a fault tolerance that is greater than
  // this initial fault weight combined with our fault tolerance threshold t.
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float]
  def lastFinalizedBlock: F[BlockMessage]
  def latestScope: F[MessageScope[BlockMetadata]]
  def getRuntimeManager: F[RuntimeManager[F]]
}

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
  def ignoreDoppelgangerCheck[F[_]: Applicative]: (BlockMessage, Validator) => F[Unit] =
    kp2(().pure)
}

/**
  * Casper snapshot contains prepared data for validation/proposing. It is pure and does not have access to any effects.
  */
final case class CasperSnapshot(
    networkState: NetworkSnapshot,
    conflictScopeResolution: ConflictResolution[DeployChain],
    snapshotState: StateHash,
    maxBlockNum: Long,
    maxSeqNums: Map[Validator, Int],
    deployLifespan: Int,
    shardNam: String,
    casperVersion: Int
)

final case class OnChainCasperState(
    shardConf: CasperShardConf,
    bondsMap: Map[Validator, Long],
    activeValidators: Seq[Validator]
)

final case class CasperShardConf(
    faultToleranceThreshold: Float,
    shardName: String,
    parentShardId: String,
    finalizationRate: Int,
    maxNumberOfParents: Int,
    maxParentDepth: Int,
    synchronyConstraintThreshold: Float,
    heightConstraintThreshold: Long,
    // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
    // Required to enable protection from re-submitting duplicate deploys
    deployLifespan: Int,
    casperVersion: Long,
    configVersion: Long,
    bondMinimum: Long,
    bondMaximum: Long,
    epochLength: Int,
    quarantineLength: Int
)

sealed abstract class MultiParentCasperInstances {
  implicit val MetricsSource: Metrics.Source =
    Metrics.Source(CasperMetricsSource, "casper")

  def hashSetCasper[F[_]: Sync: Metrics: Concurrent: CommUtil: Log: Time: BlockStore: BlockDagStorage: Span: EventPublisher: DeployStorage: BlockRetriever](
      validatorId: Option[ValidatorIdentity],
      shard: String,
      faultToleranceThreshold: Float
  )(implicit runtimeManager: RuntimeManager[F]): F[MultiParentCasper[F]] =
    new MultiParentCasperImpl(
      validatorId,
      faultToleranceThreshold,
      shard
    ).asInstanceOf[MultiParentCasper[F]].pure[F]
}
