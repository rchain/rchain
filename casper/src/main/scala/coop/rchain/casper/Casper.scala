package coop.rchain.casper

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import cats.{Applicative, Monad, Show}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.engine.{BlockRetriever, Running}
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, MetricsSemaphore, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.shared._
import coop.rchain.shared.syntax._

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
  def contains(hash: BlockHash): F[Boolean]
  def dagContains(hash: BlockHash): F[Boolean]
  def bufferContains(hash: BlockHash): F[Boolean]
  def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]]
  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]]
  def getApprovedBlock: F[BlockMessage]
  def getValidator: F[Option[ValidatorIdentity]]
  def getVersion: F[Long]

  def validate(b: BlockMessage, s: CasperSnapshot[F]): F[Either[BlockError, ValidBlock]]
  def handleValidBlock(block: BlockMessage): F[BlockDagRepresentation[F]]
  def handleInvalidBlock(
      block: BlockMessage,
      status: InvalidBlock,
      dag: BlockDagRepresentation[F]
  ): F[BlockDagRepresentation[F]]
  def getDependencyFreeFromBuffer: F[List[BlockMessage]]
}

object Casper {

  /**
    * Casper messages serve only two purposes:
    * 1. offering state transitions for validation by the network, and
    * 2. participating in past state transition finalization.
    * Therefore message producing makes sense and should be allowed only if it is justified by these reasons.
    *
    * Given said above here are rules for detecting if propose is justified:
    * 1. State transition
    *    a) there is a user deploy to be included in a block, or
    *    b) parents have different post state, so merge block should be issued.
    *    Both of these rules can be validated, so offence an be detected.
    * 2. Finalization (reaching acquiescence)
    *    There are non finalized messages which do not share post state with some finalized message.
    *    This rule also can be validated and offence detected.
    *    TODO this can be narrowed down to "if sender already propagated its weight down to all states known"
    */
  def shouldPropose[F[_]: Monad](
      s: CasperSnapshot[F],
      deploysWaiting: F[Set[Signed[DeployData]]]
  ): F[Boolean] = {
    val hasParentsToMerge = (s.parents.map(_.body.state.postStateHash).distinct.size > 1).pure[F]
    val hasDeploysToOffer = deploysWaiting.map(_ -- s.deploysInScope).map(_.nonEmpty)
    val noAcquiescence    = s.dag.reachedAcquiescence.not
    hasDeploysToOffer ||^ hasParentsToMerge ||^ noAcquiescence
  }
}

trait MultiParentCasper[F[_]] extends Casper[F] {
  def blockDag: F[BlockDagRepresentation[F]]
  def fetchDependencies: F[Unit]
  // This is the weight of faults that have been accumulated so far.
  // We want the clique oracle to give us a fault tolerance that is greater than
  // this initial fault weight combined with our fault tolerance threshold t.
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float]
  def lastFinalizedBlock: F[BlockMessage]
  def getRuntimeManager: F[RuntimeManager[F]]
}

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
  def ignoreDoppelgangerCheck[F[_]: Applicative]: (BlockMessage, Validator) => F[Unit] =
    kp2(().pure)
}

/**
  * Casper snapshot is a state that is changing in discrete manner with each new block added.
  * This class represents full information about the state. It is required for creating new blocks
  * as well as for validating blocks.
  */
final case class CasperSnapshot[F[_]](
    dag: BlockDagRepresentation[F],
    lastFinalizedBlock: BlockHash,
    parents: List[BlockMessage],
    justifications: Set[Justification],
    invalidBlocks: Map[Validator, BlockHash],
    deploysInScope: Set[Signed[DeployData]],
    maxBlockNum: Long,
    maxSeqNums: Map[Validator, Int],
    onChainState: OnChainCasperState
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
    quarantineLength: Int,
    minPhloPrice: Long
)

sealed abstract class MultiParentCasperInstances {
  implicit val MetricsSource: Metrics.Source =
    Metrics.Source(CasperMetricsSource, "casper")

  def hashSetCasper[F[_]: Sync: Metrics: Concurrent: CommUtil: Log: Time: Timer: SafetyOracle: BlockStore: BlockDagStorage: Span: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Estimator: DeployStorage: CasperBufferStorage: BlockRetriever](
      validatorId: Option[ValidatorIdentity],
      casperShardConf: CasperShardConf,
      approvedBlock: BlockMessage
  )(implicit runtimeManager: RuntimeManager[F]): F[MultiParentCasper[F]] =
    for {
      _ <- ().pure
    } yield {
      new MultiParentCasperImpl(
        validatorId,
        casperShardConf,
        approvedBlock
      )
    }
}
