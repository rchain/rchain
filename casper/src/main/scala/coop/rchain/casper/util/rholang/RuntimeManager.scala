package coop.rchain.casper.util.rholang

import cats.Parallel
import cats.data.EitherT
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.RuntimeManager.{MergeableStore, StateHash}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoHistoryRepository, RhoISpace, RhoReplayISpace}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.{
  deployMergeableDataSeqCodec,
  DeployMergeableData
}
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rspace
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{RSpace, ReplayRSpace}
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import coop.rchain.models.syntax._
import coop.rchain.shared.{Base16, Log}
import retry.RetryDetails.{GivingUp, WillDelayAndRetry}
import retry._
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

trait RuntimeManager[F[_]] {
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
  def getMergeableStore: MergeableStore[F]
}

final case class RuntimeManagerImpl[F[_]: Concurrent: Metrics: Span: Log: ContextShift: Parallel](
    space: RhoISpace[F],
    replaySpace: RhoReplayISpace[F],
    historyRepo: RhoHistoryRepository[F],
    mergeableStore: MergeableStore[F],
    mergeableTagName: Par
) extends RuntimeManager[F] {

  def spawnRuntime: F[RhoRuntime[F]] =
    for {
      newSpace <- space
                   .asInstanceOf[
                     RSpace[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]
                   ]
                   .spawn
      runtime <- RhoRuntime.createRhoRuntime(newSpace, mergeableTagName)
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
      runtime <- RhoRuntime.createReplayRhoRuntime(newReplaySpace, mergeableTagName)
    } yield runtime

  def computeState(startHash: StateHash)(
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    for {
      runtime                                 <- spawnRuntime
      computed                                <- runtime.computeState(startHash, terms, systemDeploys, blockData, invalidBlocks)
      (stateHash, usrDeployRes, sysDeployRes) = computed
      (usrProcessed, usrMergeable)            = usrDeployRes.unzip
      (sysProcessed, sysMergeable)            = sysDeployRes.unzip

      // Concat user and system deploys mergeable channel maps
      mergeableChs = usrMergeable ++ sysMergeable

      // Block data used for mergeable key
      BlockData(_, _, sender, seqNum) = blockData
      // Convert from final to diff values and persist mergeable (number) channels for post-state hash
      preStateHash  = startHash.toBlake2b256Hash
      postSTateHash = stateHash.toBlake2b256Hash
      _ <- this
            .saveMergeableChannels(postSTateHash, sender.bytes, seqNum, mergeableChs, preStateHash)
    } yield (stateHash, usrProcessed, sysProcessed)

  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long,
      blockNumber: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] =
    spawnRuntime
      .flatMap(_.computeGenesis(terms, blockTime, blockNumber))
      .flatMap {
        case (preState, stateHash, processed) =>
          val (processedDeploys, mergeableChs) = processed.unzip

          // Convert from final to diff values and persist mergeable (number) channels for post-state hash
          val preStateHash  = preState.toBlake2b256Hash
          val postStateHash = stateHash.toBlake2b256Hash
          this
            .saveMergeableChannels(postStateHash, Array(), seqNum = 0, mergeableChs, preStateHash)
            .as((preState, stateHash, processedDeploys))
      }

  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    spawnReplayRuntime.flatMap { replayRuntime =>
      val replayOp = replayRuntime
        .replayComputeState(startHash)(terms, systemDeploys, blockData, invalidBlocks, isGenesis)
      EitherT(replayOp).semiflatMap {
        case (stateHash, mergeableChs) =>
          // Block data used for mergeable key
          val BlockData(_, _, sender, seqNum) = blockData
          // Convert from final to diff values and persist mergeable (number) channels for post-state hash
          val preStateHash = startHash.toBlake2b256Hash
          this
            .saveMergeableChannels(stateHash, sender.bytes, seqNum, mergeableChs, preStateHash)
            .as(stateHash.toByteString)
      }.value
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

  def getMergeableStore: MergeableStore[F] = mergeableStore
}

object RuntimeManager {

  type StateHash = ByteString

  type MergeableStore[F[_]] = KeyValueTypedStore[F, ByteVector, Seq[DeployMergeableData]]

  /**
    * This is a hard-coded value for `emptyStateHash` which is calculated by
    * [[coop.rchain.casper.rholang.RuntimeOps.emptyStateHash]].
    * Because of the value is actually the same all
    * the time. For some situations, we can just use the value directly for better performance.
    */
  val emptyStateHashFixed: StateHash =
    "d4d6befb45c3cc475a5906b73c45f9453a1df0924f688606251e196780a3e876".unsafeHexToByteString

  def apply[F[_]](implicit F: RuntimeManager[F]): F.type = F

  def apply[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      rSpace: RhoISpace[F],
      replayRSpace: RhoReplayISpace[F],
      historyRepo: RhoHistoryRepository[F],
      mergeableStore: MergeableStore[F],
      mergeableTagName: Par
  ): F[RuntimeManagerImpl[F]] =
    Sync[F].delay(
      RuntimeManagerImpl(rSpace, replayRSpace, historyRepo, mergeableStore, mergeableTagName)
    )

  def apply[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      store: RSpaceStore[F],
      mergeableStore: MergeableStore[F],
      mergeableTagName: Par
  )(implicit ec: ExecutionContext): F[RuntimeManagerImpl[F]] =
    createWithHistory(store, mergeableStore, mergeableTagName).map(_._1)

  def createWithHistory[F[_]: Concurrent: ContextShift: Parallel: Metrics: Span: Log](
      store: RSpaceStore[F],
      mergeableStore: MergeableStore[F],
      mergeableTagName: Par
  )(implicit ec: ExecutionContext): F[(RuntimeManagerImpl[F], RhoHistoryRepository[F])] = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

    RSpace
      .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
      .flatMap {
        case (rSpacePlay, rSpaceReplay) =>
          val historyRepo = rSpacePlay.historyRepo
          RuntimeManager[F](rSpacePlay, rSpaceReplay, historyRepo, mergeableStore, mergeableTagName)
            .map((_, historyRepo))
      }
  }

  /**
    * Creates connection to [[MergeableStore]] database.
    *
    * Mergeable (number) channels store is used in [[RuntimeManager]] implementation.
    * This function provides default instantiation.
    */
  def mergeableStore[F[_]: Sync](kvm: KeyValueStoreManager[F]): F[MergeableStore[F]] =
    kvm.database[ByteVector, Seq[DeployMergeableData]](
      "mergeable-channel-cache",
      scodec.codecs.bytes,
      deployMergeableDataSeqCodec
    )
}
