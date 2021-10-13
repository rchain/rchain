package coop.rchain.casper.util.rholang

import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.InvalidBlock.InvalidRejectedDeploy
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.compiler.ParBuilder
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Coeval

import scala.collection.Seq

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private[this] val ComputeDeploysCheckpointMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-deploys-checkpoint")

  private[this] val ReplayBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "replay-block")

  def mkTerm[Env](rho: String, normalizerEnv: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, normalizerEnv.toEnv).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Concurrent: Log: BlockStore: Span: Metrics](
      block: BlockMessage,
      s: CasperSnapshot[F],
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = ProtoUtil.preStateHash(block)
    val rejectedDeployIds    = s.conflictResolution.rejectedSet.flatMap(_.deploys)
    if (incomingPreStateHash != s.state) {
      //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
      Log[F]
        .warn(
          s"Computed pre-state hash ${PrettyPrinter.buildString(s.state)} does not equal block's pre-state hash ${PrettyPrinter
            .buildString(incomingPreStateHash)}"
        )
        .as(none[StateHash].asRight[BlockError])
    } else if (rejectedDeployIds != block.body.rejectedDeploys.map(_.sig).toSet) {
      Log[F]
        .warn(
          s"Computed rejected deploys " +
            s"${rejectedDeployIds.toList.map(PrettyPrinter.buildString).mkString(",")} does not equal " +
            s"block's rejected deploy " +
            s"${block.body.rejectedDeploys.map(_.sig).map(PrettyPrinter.buildString).mkString(",")}"
        )
        .as(InvalidRejectedDeploy.asLeft)
    } else {
      for {
        replayResult <- replayBlock(
                         incomingPreStateHash,
                         block,
                         s.dag,
                         runtimeManager
                       )
        result <- handleErrors(ProtoUtil.postStateHash(block), replayResult)
      } yield result
    }

  }

  private def replayBlock[F[_]: Sync: Log: BlockStore](
      initialStateHash: StateHash,
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  )(implicit spanF: Span[F]): F[Either[ReplayFailure, StateHash]] =
    spanF.trace(ReplayBlockMetricsSource) {
      val internalDeploys       = ProtoUtil.deploys(block)
      val internalSystemDeploys = ProtoUtil.systemDeploys(block)
      for {
        invalidBlocksSet <- dag.invalidBlocks
        unseenBlocksSet  <- ProtoUtil.unseenBlockHashes(dag, block)
        seenInvalidBlocksSet = invalidBlocksSet.filterNot(
          block => unseenBlocksSet.contains(block.blockHash)
        ) // TODO: Write test in which switching this to .filter makes it fail
        invalidBlocks = seenInvalidBlocksSet
          .map(block => (block.blockHash, block.sender))
          .toMap
        _         <- Span[F].mark("before-process-pre-state-hash")
        blockData = BlockData.fromBlock(block)
        isGenesis = block.justifications.isEmpty
        replayResult <- runtimeManager.replayComputeState(initialStateHash)(
                         internalDeploys,
                         internalSystemDeploys,
                         blockData,
                         invalidBlocks,
                         isGenesis
                       )
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

  def computeDeploysCheckpoint[F[_]: Concurrent: BlockStore: Log: Metrics](
      deploys: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      runtimeManager: RuntimeManager[F],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator],
      preStateHash: StateHash
  )(
      implicit spanF: Span[F]
  ): F[
    (StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])
  ] =
    spanF.trace(ComputeDeploysCheckpointMetricsSource) {
      for {
        result <- runtimeManager.computeState(preStateHash)(
                   deploys,
                   systemDeploys,
                   blockData,
                   invalidBlocks
                 )
        (postStateHash, processedDeploys, processedSystemDeploys) = result
      } yield (
        postStateHash,
        processedDeploys,
        processedSystemDeploys
      )
    }
}
