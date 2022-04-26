package coop.rchain.casper

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import cats.{Applicative, Show}
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
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
  def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]]
  def getValidator: F[Option[ValidatorIdentity]]
}

trait MultiParentCasper[F[_]] extends Casper[F] {
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
final case class CasperSnapshot(
    dag: DagRepresentation,
    lastFinalizedBlock: BlockHash,
    lca: BlockHash,
    tips: IndexedSeq[BlockHash],
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

  def hashSetCasper[F[_]: Sync: Metrics: Concurrent: RuntimeManager: CommUtil: Log: Time: Timer: BlockStore: BlockDagStorage: Span: EventPublisher: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: DeployStorage: CasperBufferStorage: BlockRetriever](
      validatorId: Option[ValidatorIdentity],
      casperShardConf: CasperShardConf
  ): F[MultiParentCasper[F]] =
    for {
      _ <- ().pure
    } yield {
      new MultiParentCasperImpl(
        validatorId,
        casperShardConf.shardName,
        casperShardConf.minPhloPrice,
        casperShardConf.maxNumberOfParents
      )
    }
}
