package coop.rchain.casper.util.rholang

import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.InvalidBlock.InvalidRejectedDeploy
import coop.rchain.casper._
import coop.rchain.casper.blocks.merger.{CasperMergingDagReader, MergingVertex}
import coop.rchain.casper.merging.{BlockIndex, DagMerger}
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
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
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Coeval

import scala.collection.Seq

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
    ParBuilder[Coeval].buildNormalizedTerm(rho, normalizerEnv.toEnv).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Concurrent: Log: BlockStore: Span: Metrics](
      block: BlockMessage,
      s: CasperSnapshot[F],
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = ProtoUtil.preStateHash(block)
    for {
      _                   <- Span[F].mark("before-unsafe-get-parents")
      parents             <- ProtoUtil.getParents(block)
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
    } yield result
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
        isGenesis = block.header.parentsHashList.isEmpty
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
      parents: Seq[BlockMessage],
      deploys: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      s: CasperSnapshot[F],
      runtimeManager: RuntimeManager[F],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  )(
      implicit spanF: Span[F]
  ): F[
    (StateHash, StateHash, Seq[ProcessedDeploy], Seq[ByteString], Seq[ProcessedSystemDeploy])
  ] =
    spanF.trace(ComputeDeploysCheckpointMetricsSource) {
      for {
        nonEmptyParents <- parents.pure
                            .ensure(new IllegalArgumentException("Parents must not be empty"))(
                              _.nonEmpty
                            )
        computedParentsInfo             <- computeParentsPostState(nonEmptyParents, s, runtimeManager)
        (preStateHash, rejectedDeploys) = computedParentsInfo
        result <- runtimeManager.computeState(preStateHash)(
                   deploys,
                   systemDeploys,
                   blockData,
                   invalidBlocks
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

  private def computeParentsPostState[F[_]: Concurrent: BlockStore: Log: Metrics](
      parents: Seq[BlockMessage],
      s: CasperSnapshot[F],
      runtimeManager: RuntimeManager[F]
  )(implicit spanF: Span[F]): F[(StateHash, Seq[ByteString])] =
    spanF.trace(ComputeParentPostStateMetricsSource) {
      parents match {
        // For genesis, use empty trie's root hash
        case Seq() =>
          (RuntimeManager.emptyStateHashFixed, Seq.empty[ByteString]).pure[F]

        // For single parent, get itd post state hash
        case Seq(parent) =>
          (ProtoUtil.postStateHash(parent), Seq.empty[ByteString]).pure[F]

        // we might want to take some data from the parent with the most stake,
        // e.g. bonds map, slashing deploys, bonding deploys.
        // such system deploys are not mergeable, so take them from one of the parents.
        case parents => {
          val tipsHashes = parents.map(_.blockHash)
          val lfbHash    = s.lastFinalizedBlock
          for {
            tips <- BlockStore[F]
                     .getUnsafe(tipsHashes)
                     .map(
                       b =>
                         MergingVertex(
                           b.blockHash,
                           b.body.state.postStateHash,
                           b.body.state.preStateHash,
                           b.body.deploys.toSet
                         )
                     )
                     .compile
                     .toList
            base <- BlockStore[F]
                     .getUnsafe(lfbHash)
                     .map(
                       b =>
                         MergingVertex(
                           b.blockHash,
                           b.body.state.postStateHash,
                           b.body.state.preStateHash,
                           b.body.deploys.toSet
                         )
                     )
            blockIndex = (v: MergingVertex) => {
              val cached = BlockIndex.cache.get(v.blockHash)
              cached match {
                case Some(value) => value.pure[F]
                case None =>
                  for {
                    b <- BlockStore[F].getUnsafe(v.blockHash)
                    index <- BlockIndex(
                              b.blockHash,
                              b.body.deploys,
                              Blake2b256Hash.fromByteString(b.body.state.preStateHash),
                              Blake2b256Hash.fromByteString(b.body.state.postStateHash),
                              runtimeManager.getHistoryRepo
                            )
                  } yield index
              }
            }
            r <- DagMerger.merge[F, MergingVertex](
                  tips,
                  base,
                  new CasperMergingDagReader(s.dag),
                  v => s.dag.isFinalized(v.blockHash),
                  v => blockIndex(v).map(_.deployChains),
                  v => Blake2b256Hash.fromByteString(v.postStateHash),
                  runtimeManager.getHistoryRepo
                )
          } yield r
        }
      }
    }
}
