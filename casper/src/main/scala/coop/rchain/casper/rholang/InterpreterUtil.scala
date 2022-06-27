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
import coop.rchain.casper.rholang.RuntimeManager.{emptyStateHashFixed, StateHash}
import coop.rchain.casper.rholang.types._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.implicits.fromGUnforgeable
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, LogSource}
import retry.{retryingOnFailures, RetryPolicies}

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private[this] val ComputeDeploysCheckpointMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-deploys-checkpoint")

  private[this] val ComputeParentPostStateMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-parents-post-state")

  private[this] val ReplayBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "replay-block")

  def mkTerm[F[_]: Sync, Env](rho: String, normalizerEnv: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): F[Par] = Compiler[F].sourceToADT(rho, normalizerEnv.toEnv)

  // TODO: most of this function is legacy code, it should be refactored with separation of errors that are
  //  handled (with included data e.g. hash not equal) and fatal errors which should NOT be handled
  def validateBlockCheckpointNew[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage
  ): F[(BlockMetadata, BlockProcessing[Option[StateHash]])] = {
    val incomingPreStateHash = block.preStateHash
    for {
      _ <- Span[F].mark("before-unsafe-get-parents")
      parents <- block.justifications
                  .traverse(BlockDagStorage[F].lookupUnsafe(_))
                  .map(_.filter(!_.invalid))
                  .map(_.map(_.blockHash))

      _          <- Span[F].mark("before-compute-parents-post-state")
      parentsSet = parents.toSet
      preState <- if (parentsSet.nonEmpty)
                   MultiParentCasper.getPreStateForParents(parents.toSet)
                 else {
                   // Genesis block
                   ParentsMergedState(
                     justifications = Set[BlockMetadata](),
                     fringe = Set[BlockHash](),
                     fringeState = RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
                     // TODO: validate with data from bonds file
                     bondsMap = block.bonds,
                     rejectedDeploys = Set[ByteString](),
                     // TODO: validate with data from config (genesis block number)
                     maxBlockNum = 0L,
                     // TODO: validate with sender in bonds map
                     maxSeqNums = Map[Validator, Long](block.sender -> 0L)
                   ).pure[F]
                 }
      computedParentsInfo <- computeParentsPostState(parents, preState)
      _ <- Log[F].info(
            s"Computed parents post state for ${PrettyPrinter.buildString(block, short = true)}."
          )
      result <- {
        val (computedPreStateHash, rejectedDeployIds) = computedParentsInfo
        if (incomingPreStateHash != computedPreStateHash) {
          //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
          Log[F]
            .warn(
              s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                .buildString(incomingPreStateHash)}"
            )
            .as(none[StateHash].asRight[BlockError])
        } else if (rejectedDeployIds.toSet != block.rejectedDeploys.toSet) {
          // TODO: if rejected deploys are different that almost certain
          //  hashes doesn't match also so this branch is unreachable
          Log[F]
            .warn(
              s"Computed rejected deploys " +
                s"${rejectedDeployIds.map(PrettyPrinter.buildString).mkString(",")} does not equal " +
                s"block's rejected deploy " +
                s"${block.rejectedDeploys.map(PrettyPrinter.buildString).mkString(",")}"
            )
            .as(InvalidRejectedDeploy.asLeft)
        } else {
          for {
            replayResult <- replayBlock(incomingPreStateHash, block)
            result       <- handleErrors(block.postStateHash, replayResult)
          } yield result
        }
      }
    } yield {
      val bmd = BlockMetadata
        .fromBlock(block)
        .copy(
          validated = true,
          invalid = result.isLeft || result.right.get.isEmpty,
          finalized = false,
          fringe = preState.fringe.toList,
          fringeStateHash = preState.fringeState.bytes.toArray.toByteString
        )
      (bmd, result)
    }
  }

  // TODO: this is legacy code, it should be refactored with separation of errors that are
  //  handled (with included data e.g. hash not equal) and fatal errors which should NOT be handled
  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = block.preStateHash
    for {
      _ <- Span[F].mark("before-unsafe-get-parents")
      parents <- block.justifications
                  .traverse(BlockDagStorage[F].lookupUnsafe(_))
                  .map(_.filter(!_.invalid))
                  .map(_.map(_.blockHash))

      _          <- Span[F].mark("before-compute-parents-post-state")
      parentsSet = parents.toSet
      preState <- if (parentsSet.nonEmpty)
                   MultiParentCasper.getPreStateForParents(parents.toSet)
                 else {
                   // Genesis block
                   ParentsMergedState(
                     justifications = Set[BlockMetadata](),
                     fringe = Set[BlockHash](),
                     fringeState = RuntimeManager.emptyStateHashFixed.toBlake2b256Hash,
                     // TODO: validate with data from bonds file
                     bondsMap = block.bonds,
                     rejectedDeploys = Set[ByteString](),
                     // TODO: validate with data from config (genesis block number)
                     maxBlockNum = 0L,
                     // TODO: validate with sender in bonds map
                     maxSeqNums = Map[Validator, Long](block.sender -> 0L)
                   ).pure[F]
                 }
      computedParentsInfo <- computeParentsPostState(parents, preState)
      _ <- Log[F].info(
            s"Computed parents post state for ${PrettyPrinter.buildString(block, short = true)}."
          )
      result <- {
        val (computedPreStateHash, rejectedDeployIds) = computedParentsInfo
        if (incomingPreStateHash != computedPreStateHash) {
          //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
          Log[F]
            .warn(
              s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                .buildString(incomingPreStateHash)}"
            )
            .as(none[StateHash].asRight[BlockError])
        } else if (rejectedDeployIds.toSet != block.rejectedDeploys.toSet) {
          // TODO: if rejected deploys are different that almost certain
          //  hashes doesn't match also so this branch is unreachable
          Log[F]
            .warn(
              s"Computed rejected deploys " +
                s"${rejectedDeployIds.map(PrettyPrinter.buildString).mkString(",")} does not equal " +
                s"block's rejected deploy " +
                s"${block.rejectedDeploys.map(PrettyPrinter.buildString).mkString(",")}"
            )
            .as(InvalidRejectedDeploy.asLeft)
        } else {
          for {
            replayResult <- replayBlock(incomingPreStateHash, block)
            result       <- handleErrors(block.postStateHash, replayResult)
          } yield result
        }
      }
    } yield result
  }

  private def replayBlock[F[_]: Sync: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Span](
      initialStateHash: StateHash,
      block: BlockMessage
  ): F[Either[ReplayFailure, StateHash]] =
    Span[F].trace(ReplayBlockMetricsSource) {
      val internalDeploys       = block.state.deploys
      val internalSystemDeploys = block.state.systemDeploys
      for {
        _                  <- Span[F].mark("before-process-pre-state-hash")
        blockData          = BlockData.fromBlock(block)
        withCostAccounting = block.justifications.nonEmpty
        rand               = BlockRandomSeed.fromBlock(block)
        replayResultF = RuntimeManager[F]
          .replayComputeState(initialStateHash)(
            internalDeploys,
            internalSystemDeploys,
            rand,
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
          case InternalError(cause) =>
            new Exception(
              s"Internal errors encountered while processing deploy: ${cause.getMessage}",
              cause
            ).raiseError
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

  def computeDeploysCheckpoint[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      deploys: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      rand: Blake2b512Random,
      blockData: BlockData,
      computedParentsInfo: (StateHash, Seq[ByteString])
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy], Seq[ByteString], Seq[ProcessedSystemDeploy])] =
    Span[F].trace(ComputeDeploysCheckpointMetricsSource) {
      val (preStateHash, rejectedDeploys) = computedParentsInfo
      for {
        result <- RuntimeManager[F].computeState(preStateHash)(
                   deploys,
                   systemDeploys,
                   rand,
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

  def computeParentsPostState[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      parents: Seq[BlockHash],
      preState: ParentsMergedState
  ): F[(StateHash, Seq[ByteString])] =
    Span[F].trace(ComputeParentPostStateMetricsSource) {
      parents match {
        // For genesis, use empty trie's root hash
        case Seq() =>
          (RuntimeManager.emptyStateHashFixed, Seq.empty[ByteString]).pure[F]

        // For single parent, get itd post state hash
        case Seq(parent) =>
          BlockStore[F].getUnsafe(parent).map(_.postStateHash).map((_, Seq.empty[ByteString]))

        // Multi-parent merging on finalized fringe as a base state
        case _ =>
          for {
            dag <- BlockDagStorage[F].getRepresentation
            r <- DagMerger.merge[F](
                  dag,
                  preState.fringe.toSeq,
                  preState.fringeState,
                  getBlockIndex(_).map(_.deployChains),
                  RuntimeManager[F].getHistoryRepo,
                  DagMerger.costOptimalRejectionAlg
                )
            (state, rejected) = r
          } yield (ByteString.copyFrom(state.bytes.toArray), rejected)
      }
    }

  def getBlockIndex[F[_]: Concurrent: RuntimeManager: BlockStore](
      blockHash: BlockHash
  ): F[BlockIndex] = {
    val cached = BlockIndex.cache.get(blockHash).map(_.pure)
    cached.getOrElse {
      for {
        b         <- BlockStore[F].getUnsafe(blockHash)
        preState  = b.preStateHash
        postState = b.postStateHash
        sender    = b.sender.toByteArray
        seqNum    = b.seqNum

        mergeableChs <- RuntimeManager[F].loadMergeableChannels(postState, sender, seqNum)

        blockIndex <- BlockIndex(
                       b.blockHash,
                       b.state.deploys,
                       b.state.systemDeploys,
                       preState.toBlake2b256Hash,
                       postState.toBlake2b256Hash,
                       RuntimeManager[F].getHistoryRepo,
                       mergeableChs
                     )
      } yield blockIndex
    }
  }

}
