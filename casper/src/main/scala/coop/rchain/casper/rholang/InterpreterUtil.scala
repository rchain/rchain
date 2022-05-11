package coop.rchain.casper.rholang

import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.InvalidBlock.InvalidRejectedDeploy
import coop.rchain.casper._
import coop.rchain.casper.merging.{BlockIndex, DagMerger}
import coop.rchain.casper.protocol.{
  BlockMessage,
  DeployData,
  ProcessedDeploy,
  ProcessedSystemDeploy
}
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.types._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.syntax._
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Coeval
import retry.{retryingOnFailures, RetryPolicies}

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private[this] val ComputeDeploysCheckpointMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-deploys-checkpoint")

  private[this] val ComputeParentPostStateMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-parents-post-state")

  private[this] val ReplayBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "replay-block")

  def mkTerm[Env](rho: String, normalizerEnv: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): Either[Throwable, Par] =
    Compiler[Coeval].sourceToADT(rho, normalizerEnv.toEnv).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Concurrent: Log: BlockStore: BlockDagStorage: Span: Metrics: Timer](
      block: BlockMessage,
      s: CasperSnapshot,
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = block.preStateHash
    for {
      _ <- Span[F].mark("before-unsafe-get-parents")
      // TODO: filter invalid justifications
      parents             = block.justifications
      _                   <- Span[F].mark("before-compute-parents-post-state")
      computedParentsInfo <- computeParentsPostState(parents, s, runtimeManager).attempt
      _                   <- Log[F].info(s"Computed parents post state for ${PrettyPrinter.buildString(block)}.")
      result <- computedParentsInfo match {
                 case Left(ex) =>
                   BlockStatus.exception(ex).asLeft[Option[StateHash]].pure
                 case Right((computedPreStateHash, rejectedDeploys @ _)) =>
                   val rejectedDeployIds = rejectedDeploys.toSet
                   if (incomingPreStateHash != computedPreStateHash) {
                     //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
                     Log[F]
                       .warn(
                         s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                           .buildString(incomingPreStateHash)}"
                       )
                       .as(none[StateHash].asRight[BlockError])
                   } else if (rejectedDeployIds != block.body.rejectedDeploys.map(_.sig).toSet) {
                     Log[F]
                       .warn(
                         s"Computed rejected deploys " +
                           s"${rejectedDeployIds.map(PrettyPrinter.buildString).mkString(",")} does not equal " +
                           s"block's rejected deploy " +
                           s"${block.body.rejectedDeploys.map(_.sig).map(PrettyPrinter.buildString).mkString(",")}"
                       )
                       .as(InvalidRejectedDeploy.asLeft)
                   } else {
                     for {
                       replayResult <- replayBlock(incomingPreStateHash, block, runtimeManager)
                       result       <- handleErrors(block.postStateHash, replayResult)
                     } yield result
                   }
               }
    } yield result
  }

  private def replayBlock[F[_]: Sync: Log: BlockStore: BlockDagStorage: Timer](
      initialStateHash: StateHash,
      block: BlockMessage,
      runtimeManager: RuntimeManager[F]
  )(implicit spanF: Span[F]): F[Either[ReplayFailure, StateHash]] =
    spanF.trace(ReplayBlockMetricsSource) {
      val internalDeploys       = ProtoUtil.deploys(block)
      val internalSystemDeploys = ProtoUtil.systemDeploys(block)
      for {
        _                  <- Span[F].mark("before-process-pre-state-hash")
        blockData          = BlockData.fromBlock(block)
        withCostAccounting = block.justifications.nonEmpty
        replayResultF = runtimeManager
          .replayComputeState(initialStateHash)(
            internalDeploys,
            internalSystemDeploys,
            blockData,
            withCostAccounting
          )
        replayResult <- retryingOnFailures[Either[ReplayFailure, StateHash]](
                         RetryPolicies.limitRetries(3), {
                           case Right(stateHash) => stateHash == block.postStateHash
                           case _                => false
                         },
                         (e, retryDetails) =>
                           e match {
                             case Right(stateHash) =>
                               Log[F].error(
                                 s"Replay block ${PrettyPrinter.buildStringNoLimit(block.blockHash)} with " +
                                   s"${PrettyPrinter.buildStringNoLimit(block.postStateHash)} " +
                                   s"got tuple space mismatch error with error hash ${PrettyPrinter
                                     .buildStringNoLimit(stateHash)}, retries details: ${retryDetails}"
                               )
                             case Left(replayError) =>
                               Log[F].error(
                                 s"Replay block ${PrettyPrinter.buildStringNoLimit(block.blockHash)} got " +
                                   s"error ${replayError}, retries details: ${retryDetails}"
                               )
                           }
                       )(replayResultF)
      } yield replayResult
    }

  private def handleErrors[F[_]: Sync: Log](
      tsHash: ByteString,
      result: Either[ReplayFailure, StateHash]
  ): F[BlockProcessing[Option[StateHash]]] =
    result.pure.flatMap {
      case Left(status) =>
        status match {
          case InternalError(throwable) =>
            BlockStatus
              .exception(
                new Exception(
                  s"Internal errors encountered while processing deploy: ${throwable.getMessage}"
                )
              )
              .asLeft[Option[StateHash]]
              .pure
          case ReplayStatusMismatch(replayFailed, initialFailed) =>
            Log[F]
              .warn(
                s"Found replay status mismatch; replay failure is $replayFailed and orig failure is $initialFailed"
              )
              .as(none[StateHash].asRight[BlockError])
          case UnusedCOMMEvent(replayException) =>
            Log[F]
              .warn(
                s"Found replay exception: ${replayException.getMessage}"
              )
              .as(none[StateHash].asRight[BlockError])
          case ReplayCostMismatch(initialCost, replayCost) =>
            Log[F]
              .warn(
                s"Found replay cost mismatch: initial deploy cost = $initialCost, replay deploy cost = $replayCost"
              )
              .as(none[StateHash].asRight[BlockError])
          // Restructure errors so that this case is unnecessary
          case SystemDeployErrorMismatch(playMsg, replayMsg) =>
            Log[F]
              .warn(
                s"Found system deploy error mismatch: initial deploy error message = $playMsg, replay deploy error message = $replayMsg"
              )
              .as(none[StateHash].asRight[BlockError])
        }
      case Right(computedStateHash) =>
        if (tsHash == computedStateHash) {
          // state hash in block matches computed hash!
          computedStateHash.some.asRight[BlockError].pure
        } else {
          // state hash in block does not match computed hash -- invalid!
          // return no state hash, do not update the state hash set
          Log[F]
            .warn(
              s"Tuplespace hash ${PrettyPrinter.buildString(tsHash)} does not match computed hash ${PrettyPrinter
                .buildString(computedStateHash)}."
            )
            .as(none[StateHash].asRight[BlockError])
        }
    }

  /**
    * Temporary solution to print user deploy errors to Log so we can have
    * at least some way to debug user errors.
    */
  def printDeployErrors[F[_]: Sync: Log](
      deploySig: ByteString,
      errors: Seq[InterpreterError]
  ): F[Unit] = Sync[F].defer {
    val deployInfo = PrettyPrinter.buildStringSig(deploySig)
    Log[F].info(s"Deploy ($deployInfo) errors: ${errors.mkString(", ")}")
  }

  def computeDeploysCheckpoint[F[_]: Concurrent: BlockStore: BlockDagStorage: Log: Metrics](
      deploys: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      runtimeManager: RuntimeManager[F],
      blockData: BlockData,
      computedParentsInfo: (StateHash, Seq[ByteString])
  )(
      implicit spanF: Span[F]
  ): F[
    (StateHash, StateHash, Seq[ProcessedDeploy], Seq[ByteString], Seq[ProcessedSystemDeploy])
  ] =
    spanF.trace(ComputeDeploysCheckpointMetricsSource) {
      val (preStateHash, rejectedDeploys) = computedParentsInfo
      for {
        result <- runtimeManager.computeState(preStateHash)(
                   deploys,
                   systemDeploys,
                   blockData
                 )
        (postStateHash, processedDeploys, processedSystemDeploys) = result
      } yield (
        preStateHash,
        postStateHash,
        processedDeploys,
        rejectedDeploys,
        processedSystemDeploys
      )
    }

  def computeParentsPostState[F[_]: Concurrent: BlockStore: BlockDagStorage: Log: Metrics](
      parents: Seq[BlockHash],
      s: CasperSnapshot,
      runtimeManager: RuntimeManager[F]
  )(implicit spanF: Span[F]): F[(StateHash, Seq[ByteString])] =
    spanF.trace(ComputeParentPostStateMetricsSource) {
      parents match {
        // For genesis, use empty trie's root hash
        case Seq() =>
          (RuntimeManager.emptyStateHashFixed, Seq.empty[ByteString]).pure[F]

        // For single parent, get itd post state hash
        case Seq(parent) =>
          BlockStore[F]
            .getUnsafe(parent)
            .map(_.postStateHash)
            .map((_, Seq.empty[ByteString]))

        // we might want to take some data from the parent with the most stake,
        // e.g. bonds map, slashing deploys, bonding deploys.
        // such system deploys are not mergeable, so take them from one of the parents.
        case _ => {
          val blockIndexF = (v: BlockHash) => {
            val cached = BlockIndex.cache.get(v).map(_.pure)
            cached.getOrElse {
              for {
                b         <- BlockStore[F].getUnsafe(v)
                preState  = b.preStateHash
                postState = b.postStateHash
                sender    = b.sender.toByteArray
                seqNum    = b.seqNum

                mergeableChs <- runtimeManager.loadMergeableChannels(postState, sender, seqNum)

                blockIndex <- BlockIndex(
                               b.blockHash,
                               b.body.deploys,
                               b.body.systemDeploys,
                               preState.toBlake2b256Hash,
                               postState.toBlake2b256Hash,
                               runtimeManager.getHistoryRepo,
                               mergeableChs
                             )
              } yield blockIndex
            }
          }
          for {
            lfbState <- BlockStore[F]
                         .getUnsafe(s.lastFinalizedBlock)
                         .map(_.postStateHash)
                         .map(Blake2b256Hash.fromByteString)
            r <- DagMerger.merge[F](
                  s.dag,
                  s.lastFinalizedBlock,
                  lfbState,
                  blockIndexF(_).map(_.deployChains),
                  runtimeManager.getHistoryRepo,
                  DagMerger.costOptimalRejectionAlg
                )
            (state, rejected) = r
          } yield (ByteString.copyFrom(state.bytes.toArray), rejected)

        }
      }
    }
}
