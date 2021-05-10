package coop.rchain.casper.util.rholang

import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.blocks.merger.{BlockIndex, Indexer, MergingVertex}
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoHistoryRepository, RhoISpace, RhoReplayISpace}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{RSpace, ReplayRSpace}
import coop.rchain.shared.Log
import coop.rchain.store.LazyKeyValueCache
import retry.RetryDetails.{GivingUp, WillDelayAndRetry}
import retry._

import scala.concurrent.duration.FiniteDuration

trait RuntimeManager[F[_]] {
  def playSystemDeploy[S <: SystemDeploy](startHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]]
  def replaySystemDeploy[S <: SystemDeploy](startHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]]
  def captureResults(
      startHash: StateHash,
      deploy: Signed[DeployData]
  ): F[Seq[Par]]
  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator],
      isGenesis: Boolean
  ): F[Either[ReplayFailure, StateHash]]
  def computeState(hash: StateHash)(
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])]
  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])]
  def computeBonds(startHash: StateHash): F[Seq[Bond]]
  def getActiveValidators(startHash: StateHash): F[Seq[Validator]]
  def getData(hash: StateHash)(channel: Par): F[Seq[Par]]
  def getContinuation(hash: StateHash)(
      channels: Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def withRuntime[A](f: RhoRuntime[F] => F[A]): F[A]
  def withReplayRuntime[A](f: ReplayRhoRuntime[F] => F[A]): F[A]
  // Executes deploy as user deploy with immediate rollback
  def playExploratoryDeploy(term: String, startHash: StateHash): F[Seq[Par]]
  def getHistoryRepo: RhoHistoryRepository[F]
  def getBlockIndexCache: LazyKeyValueCache[F, MergingVertex, BlockIndex]
}

class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log: ContextShift: Parallel](
    space: RhoISpace[F],
    replaySpace: RhoReplayISpace[F],
    historyRepo: RhoHistoryRepository[F],
    blockIndexCache: LazyKeyValueCache[F, MergingVertex, BlockIndex]
) extends RuntimeManager[F] {

  def withRuntime[A](f: RhoRuntime[F] => F[A]): F[A] =
    for {
      newSpace <- space
                   .asInstanceOf[
                     RSpace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]
                   ]
                   .spawn
      runtime <- RhoRuntime.createRhoRuntime(newSpace)
      r       <- f(runtime)
    } yield r

  def withReplayRuntime[A](f: ReplayRhoRuntime[F] => F[A]): F[A] =
    for {
      newReplaySpace <- replaySpace
                         .asInstanceOf[ReplayRSpace[
                           F,
                           Par,
                           BindPattern,
                           ListParWithRandom,
                           TaggedContinuation
                         ]]
                         .spawn
      runtime <- RhoRuntime.createReplayRhoRuntime(newReplaySpace)
      r       <- f(runtime)
    } yield r

  def playSystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]] =
    withRuntime(runtime => runtime.playSystemDeploy(stateHash)(systemDeploy))

  // TODO: method is used only in tests
  def replaySystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]] =
    withReplayRuntime(
      replayRuntime =>
        replayRuntime.replaySystemDeploy(stateHash)(systemDeploy, processedSystemDeploy)
    )

  def computeState(startHash: StateHash)(
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    withRuntime(
      runtime => runtime.computeState(startHash, terms, systemDeploys, blockData, invalidBlocks)
    )

  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] =
    withRuntime(runtime => runtime.computeGenesis(terms, blockTime))

  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    withReplayRuntime(
      replayRuntime =>
        replayRuntime
          .replayComputeState(startHash)(terms, systemDeploys, blockData, invalidBlocks, isGenesis)
    )

  def captureResults(
      start: StateHash,
      deploy: Signed[DeployData]
  ): F[Seq[Par]] = withRuntime(runtime => runtime.captureResults(start, deploy))

  def getActiveValidators(startHash: StateHash): F[Seq[Validator]] =
    withRuntime(runtime => runtime.getActiveValidators(startHash))

  def computeBonds(hash: StateHash): F[Seq[Bond]] =
    withRuntime(runtime => {
      def logError(err: Throwable, details: RetryDetails): F[Unit] = details match {
        case WillDelayAndRetry(_, retriesSoFar: Int, _) =>
          Log[F].error(
            s"Unexpected exception ${err} during computeBonds. Retrying ${retriesSoFar + 1} time."
          )
        case GivingUp(totalRetries: Int, _) =>
          Log[F].error(
            s"Unexpected exception ${err} during computeBonds. Giving up after ${totalRetries} retries."
          )
      }

      implicit val s = new retry.Sleep[F] {
        override def sleep(delay: FiniteDuration): F[Unit] = ().pure[F]
      }

      //TODO this retry is a temp solution for debugging why this throws `IllegalArgumentException`
      retryingOnAllErrors[Seq[Bond]](
        RetryPolicies.limitRetries[F](5),
        onError = logError
      )(runtime.computeBonds(hash))
    })

  // Executes deploy as user deploy with immediate rollback
  // - InterpreterError is rethrown
  def playExploratoryDeploy(term: String, hash: StateHash): F[Seq[Par]] =
    withRuntime(runtime => runtime.playExploratoryDeploy(term, hash))

  def getData(hash: StateHash)(channel: Par): F[Seq[Par]] =
    withRuntime(
      runtime => runtime.reset(Blake2b256Hash.fromByteString(hash)) >> runtime.getDataPar(channel)
    )

  def getContinuation(
      hash: StateHash
  )(channels: Seq[Par]): F[Seq[(Seq[BindPattern], Par)]] =
    withRuntime(
      runtime =>
        runtime.reset(Blake2b256Hash.fromByteString(hash)) >> runtime.getContinuationPar(channels)
    )

  def getHistoryRepo: RhoHistoryRepository[F] = historyRepo

  override def getBlockIndexCache: LazyKeyValueCache[F, MergingVertex, BlockIndex] = blockIndexCache
}

object RuntimeManager {

  type StateHash = ByteString

  /**
    * This is a hard-coded value for `emptyStateHash` which is calculated by
    * [[coop.rchain.casper.util.rholang.RhoRuntimeOps.emptyStateHash]].
    * Because of the value is actually the same all
    * the time. For some situations, we can just use the value directly for better performance.
    */
  val emptyStateHashFixed: StateHash =
    ByteString.copyFrom(
      Base16.unsafeDecode("35e25e6e778ffc4f394a74e784be1fcebe513613ac5f95b9424619146e5d9675")
    )

  def fromRuntimes[F[_]: Concurrent: Metrics: Span: Log: Parallel: ContextShift](
      runtime: RhoRuntime[F],
      replayRuntime: ReplayRhoRuntime[F],
      historyRepo: RhoHistoryRepository[F]
  ): F[RuntimeManager[F]] =
    for {
      blockIndexCache <- LazyKeyValueCache[F, MergingVertex, BlockIndex](
                          Indexer.createBlockIndex[F]
                        )
    } yield new RuntimeManagerImpl(
      runtime.getRSpace.asInstanceOf[RhoISpace[F]],
      replayRuntime.getRSpace.asInstanceOf[RhoReplayISpace[F]],
      historyRepo,
      blockIndexCache
    )

  def apply[F[_]](implicit instance: RuntimeManager[F]): RuntimeManager[F] = instance

}
