package coop.rchain.casper.util.rholang

import cats.effect.concurrent.{MVar, MVar2}
import cats.effect.{Sync, _}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.casper.syntax._
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.Log
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
  def emptyStateHash: F[StateHash]
  def withRuntimeLock[A](f: RhoRuntime[F] => F[A]): F[A]
  // Executes deploy as user deploy with immediate rollback
  def playExploratoryDeploy(term: String, startHash: StateHash): F[Seq[Par]]
}

class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log](
    runtimeContainer: MVar2[F, RhoRuntime[F]],
    replayRuntimeContainer: MVar2[F, ReplayRhoRuntime[F]]
) extends RuntimeManager[F] {

  private[this] val RuntimeManagerMetricsSource =
    Metrics.Source(CasperMetricsSource, "runtime-manager")

  override def emptyStateHash: F[StateHash] =
    withRuntime(runtime => runtime.emptyStateHash)
  private def withRuntime[A](f: RhoRuntime[F] => F[A]): F[A] =
    Sync[F].bracket {
      import coop.rchain.metrics.implicits._
      implicit val ms: Source = RuntimeManagerMetricsSource
      for {
        _       <- Metrics[F].incrementGauge("lock.queue")
        runtime <- Sync[F].defer(runtimeContainer.take).timer("lock.acquire")
        _       <- Metrics[F].decrementGauge("lock.queue")
      } yield runtime
    }(f)(runtimeContainer.put)

  private def withReplayRuntime[A](f: ReplayRhoRuntime[F] => F[A]): F[A] =
    Sync[F].bracket {
      import coop.rchain.metrics.implicits._
      implicit val ms: Source = RuntimeManagerMetricsSource
      for {
        _       <- Metrics[F].incrementGauge("lock.queue")
        runtime <- Sync[F].defer(replayRuntimeContainer.take).timer("lock.acquire")
        _       <- Metrics[F].decrementGauge("lock.queue")
      } yield runtime
    }(f)(replayRuntimeContainer.put)

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

  def withRuntimeLock[A](f: RhoRuntime[F] => F[A]): F[A] =
    withRuntime(f)

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

}

object RuntimeManager {

  type StateHash = ByteString

//  def fromRuntime[F[_]: Concurrent: Sync: Metrics: Span: Log](
//      runtime: Runtime[F]
//  ): F[RuntimeManager[F]] =
//    for {
//      _                <- runtime.space.clear()
//      _                <- runtime.replaySpace.clear()
//      _                <- Runtime.bootstrapRegistry(runtime)
//      checkpoint       <- runtime.space.createCheckpoint()
//      replayCheckpoint <- runtime.replaySpace.createCheckpoint()
//      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
//      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
//      _                = assert(hash == replayHash)
//      runtime          <- MVar[F].of(runtime)
//    } yield new RuntimeManagerImpl(hash, runtime)

  def fromRuntimes[F[_]: Concurrent: Sync: Metrics: Span: Log](
      runtime: RhoRuntime[F],
      replayRuntime: ReplayRhoRuntime[F]
  ): F[RuntimeManager[F]] =
    for {
      runtime       <- MVar[F].of(runtime)
      replayRuntime <- MVar[F].of(replayRuntime)
    } yield new RuntimeManagerImpl(runtime, replayRuntime)

}
