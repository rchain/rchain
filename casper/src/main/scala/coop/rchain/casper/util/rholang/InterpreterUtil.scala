package coop.rchain.casper.util.rholang

import cats.data.EitherT
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.InvalidBlock.{InvalidPreStateHash, InvalidRejectedDeploy}
import coop.rchain.casper._
import coop.rchain.casper.blocks.merger.{CasperDagMerger, CasperMergingDagReader, MergingVertex}
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.compiler.ParBuilder
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
    val parents              = block.header.parentsHashList
    // use merging root specified in block or compute it
    val validateMergingRoot = false
    for {
      preState <- computeParentsPostState(
                   parents,
                   s,
                   runtimeManager,
                   if (validateMergingRoot) none else block.body.mergingSpec.mergingRoot.some
                 ).attempt
      _ <- Log[F].info(
            s"Computed parents post state for ${PrettyPrinter.buildString(block, true)}."
          )
      result <- (preState match {
                 case Left(ex) =>
                   EitherT.fromEither(BlockStatus.exception(ex).asLeft[Option[StateHash]])

                 case Right((computedPreStateHash, _ @MergingSpec(mergingRoot, rejectedDeploys))) => {
                   val preStateHashCheck =
                     if (incomingPreStateHash != computedPreStateHash) {
                       val actual   = PrettyPrinter.buildString(computedPreStateHash)
                       val expected = PrettyPrinter.buildString(incomingPreStateHash)
                       val logStr =
                         s"Computed pre-state hash $actual does not match expected $expected."
                       Log[F]
                         .warn(logStr)
                         .as(BlockStatus.invalidPreStateHash.asLeft[Unit])
                     } else ().asRight[BlockError].pure[F]

                   val mergingRootCheck =
                     if (parents.size > 1 && mergingRoot != block.body.mergingSpec.mergingRoot) {
                       val actual   = PrettyPrinter.buildString(mergingRoot)
                       val expected = PrettyPrinter.buildString(block.body.mergingSpec.mergingRoot)
                       val logStr =
                         s"Computed merging root $actual does not match expected $expected."
                       Log[F]
                         .warn(logStr)
                         .as(BlockStatus.invalidMergingRoot.asLeft[Unit])
                     } else ().asRight[BlockError].pure[F]

                   val rejectedDeploysCheck =
                     if (rejectedDeploys.toSet != block.body.mergingSpec.rejectedDeploys.toSet) {
                       val actual = rejectedDeploys.map(PrettyPrinter.buildString).mkString(",")
                       val expected = block.body.mergingSpec.rejectedDeploys
                         .map(PrettyPrinter.buildString)
                         .mkString(",")
                       val logStr =
                         s"Computed rejected deploys $actual do not match expected $expected."
                       Log[F]
                         .warn(logStr)
                         .as(BlockStatus.invalidRejectedDeploy.asLeft[Unit])
                     } else ().asRight[BlockError].pure[F]

                   val stateTransitionCheck =
                     replayBlock(incomingPreStateHash, block, s.dag, runtimeManager)
                       .flatMap(r => handleErrors(ProtoUtil.postStateHash(block), r))

                   for {
                     _ <- EitherT(mergingRootCheck)
                     _ <- EitherT(rejectedDeploysCheck)
                     _ <- EitherT(preStateHashCheck)
                     r <- EitherT(stateTransitionCheck)
                   } yield r
                 }
               }).value
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
    (StateHash, StateHash, Seq[ProcessedDeploy], MergingSpec, Seq[ProcessedSystemDeploy])
  ] =
    spanF.trace(ComputeDeploysCheckpointMetricsSource) {
      for {
        nonEmptyParents <- parents.pure
                            .ensure(new IllegalArgumentException("Parents must not be empty"))(
                              _.nonEmpty
                            )
        preState                    <- computeParentsPostState(nonEmptyParents.map(_.blockHash), s, runtimeManager)
        (preStateHash, mergingSpec) = preState
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
        mergingSpec,
        processedSystemDeploys
      )
    }

  private def computeParentsPostState[F[_]: Concurrent: BlockStore: Log: Metrics](
      parents: Seq[BlockHash],
      s: CasperSnapshot[F],
      runtimeManager: RuntimeManager[F],
      mergingRoot: Option[BlockHash] = None
  )(implicit spanF: Span[F]): F[(StateHash, MergingSpec)] =
    spanF.trace(ComputeParentPostStateMetricsSource) {
      parents match {
        // For genesis, use empty trie's root hash
        case Seq() =>
          (RuntimeManager.emptyStateHashFixed, MergingSpec.noMerge).pure[F]

        // For single parent, get its post state hash
        case Seq(p) =>
          BlockStore[F].getUnsafe(p).map(b => (ProtoUtil.postStateHash(b), MergingSpec.noMerge))

        // we might want to take some data from the parent with the most stake,
        // e.g. bonds map, slashing deploys, bonding deploys.
        // such system deploys are not mergeable, so take them from one of the parents.
        case _ => {
          implicit val r = runtimeManager
          val lfbHash    = s.lastFinalizedBlock
          for {
            tips <- BlockStore[F]
                     .getUnsafe(parents)
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
                     .getUnsafe(mergingRoot.getOrElse(lfbHash))
                     .map(
                       b =>
                         MergingVertex(
                           b.blockHash,
                           b.body.state.postStateHash,
                           b.body.state.preStateHash,
                           b.body.deploys.toSet
                         )
                     )
            r <- CasperDagMerger.merge(
                  tips,
                  base,
                  new CasperMergingDagReader(s.dag),
                  runtimeManager.getBlockIndexCache
                )
            (preStatHash, mergingRoot, rejectedDeploys) = r
          } yield (preStatHash, MergingSpec(mergingRoot, rejectedDeploys.map(_.deploy.sig)))
        }
      }
    }
}
