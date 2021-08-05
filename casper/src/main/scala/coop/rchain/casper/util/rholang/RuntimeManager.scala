package coop.rchain.casper.util.rholang

import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
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
import coop.rchain.rspace
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{RSpace, ReplayRSpace}
import coop.rchain.shared.Log
import retry.RetryDetails.{GivingUp, WillDelayAndRetry}
import retry._

import scala.concurrent.ExecutionContext
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
      blockTime: Long,
      blockNumber: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])]
  def computeBonds(startHash: StateHash): F[Seq[Bond]]
  def getActiveValidators(startHash: StateHash): F[Seq[Validator]]
  def getData(hash: StateHash)(channel: Par): F[Seq[Par]]
  def getContinuation(hash: StateHash)(
      channels: Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]]
  def spawnRuntime: F[RhoRuntime[F]]
  def spawnReplayRuntime: F[ReplayRhoRuntime[F]]
  // Executes deploy as user deploy with immediate rollback
  def playExploratoryDeploy(term: String, startHash: StateHash): F[Seq[Par]]
  def getHistoryRepo: RhoHistoryRepository[F]
}

final case class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log: ContextShift: Parallel](
    space: RhoISpace[F],
    replaySpace: RhoReplayISpace[F],
    historyRepo: RhoHistoryRepository[F]
) extends RuntimeManager[F] {

  def spawnRuntime: F[RhoRuntime[F]] =
    for {
      newSpace <- space
                   .asInstanceOf[
                     RSpace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]
                   ]
                   .spawn
      runtime <- RhoRuntime.createRhoRuntime(newSpace)
    } yield runtime

  def spawnReplayRuntime: F[ReplayRhoRuntime[F]] =
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
    } yield runtime

  def playSystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]] =
    spawnRuntime.flatMap(_.playSystemDeploy(stateHash)(systemDeploy))

  // TODO: method is used only in tests
  def replaySystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]] =
    spawnReplayRuntime.flatMap { replayRuntime =>
      replayRuntime.replaySystemDeploy(stateHash)(systemDeploy, processedSystemDeploy)
    }

  def computeState(startHash: StateHash)(
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    spawnRuntime.flatMap(_.computeState(startHash, terms, systemDeploys, blockData, invalidBlocks))

  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long,
      blockNumber: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] =
    spawnRuntime.flatMap(_.computeGenesis(terms, blockTime, blockNumber))

  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    spawnReplayRuntime.flatMap { replayRuntime =>
      replayRuntime
        .replayComputeState(startHash)(terms, systemDeploys, blockData, invalidBlocks, isGenesis)
    }

  def captureResults(
      start: StateHash,
      deploy: Signed[DeployData]
  ): F[Seq[Par]] = spawnRuntime.flatMap(_.captureResults(start, deploy))

  def getActiveValidators(startHash: StateHash): F[Seq[Validator]] =
    spawnRuntime.flatMap(_.getActiveValidators(startHash))

  def computeBonds(hash: StateHash): F[Seq[Bond]] =
    spawnRuntime.flatMap { runtime =>
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
    }

  // Executes deploy as user deploy with immediate rollback
  // - InterpreterError is rethrown
  def playExploratoryDeploy(term: String, hash: StateHash): F[Seq[Par]] =
    spawnRuntime.flatMap(_.playExploratoryDeploy(term, hash))

  def getData(hash: StateHash)(channel: Par): F[Seq[Par]] =
    spawnRuntime.flatMap { runtime =>
      runtime.reset(Blake2b256Hash.fromByteString(hash)) >> runtime.getDataPar(channel)
    }

  def getContinuation(
      hash: StateHash
  )(channels: Seq[Par]): F[Seq[(Seq[BindPattern], Par)]] =
    spawnRuntime.flatMap { runtime =>
      runtime.reset(Blake2b256Hash.fromByteString(hash)) >> runtime.getContinuationPar(channels)
    }

  def getHistoryRepo: RhoHistoryRepository[F] = historyRepo
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
      Base16.unsafeDecode("099ccd81eec17286112aba7ec7774a6b52d04acfba324ed0959f410361435844")
    )

  def apply[F[_]](implicit F: RuntimeManager[F]): F.type = F

  def apply[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      rSpace: RhoISpace[F],
      replayRSpace: RhoReplayISpace[F],
      historyRepo: RhoHistoryRepository[F]
  ): F[RuntimeManagerImpl[F]] = Sync[F].delay(RuntimeManagerImpl(rSpace, replayRSpace, historyRepo))

  def apply[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      store: RSpaceStore[F]
  )(implicit ec: ExecutionContext): F[RuntimeManagerImpl[F]] =
    createWithHistory(store).map(_._1)

  def createWithHistory[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      store: RSpaceStore[F]
  )(implicit ec: ExecutionContext): F[(RuntimeManagerImpl[F], RhoHistoryRepository[F])] = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

    RSpace
      .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
      .flatMap {
        case (rSpacePlay, rSpaceReplay) =>
          val historyRepo = rSpacePlay.historyRepo
          RuntimeManager[F](rSpacePlay, rSpaceReplay, historyRepo).map((_, historyRepo))
      }
  }
}
