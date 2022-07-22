package coop.rchain.casper.rholang

import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.InvalidBlock.InvalidRejectedDeploy
import coop.rchain.casper._
import coop.rchain.casper.merging.ParentsMergedState
import coop.rchain.casper.protocol.{
  BlockMessage,
  DeployData,
  ProcessedDeploy,
  ProcessedSystemDeploy
}
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.types._
import coop.rchain.casper.syntax._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.shared.{Log, LogSource}
import retry.{retryingOnFailures, RetryPolicies}

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  private[this] val ComputeDeploysCheckpointMetricsSource =
    Metrics.Source(CasperMetricsSource, "compute-deploys-checkpoint")

  private[this] val ReplayBlockMetricsSource =
    Metrics.Source(CasperMetricsSource, "replay-block")

  def mkTerm[F[_]: Sync, Env](rho: String, normalizerEnv: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): F[Par] = Compiler[F].sourceToADT(rho, normalizerEnv.toEnv)

  // TODO: most of this function is legacy code, it should be refactored with separation of errors that are
  //  handled (with included data e.g. hash not equal) and fatal errors which should NOT be handled
  def validateBlockCheckpoint[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage
  ): F[(BlockMetadata, BlockProcessing[Boolean])] =
    for {
      _ <- Span[F].mark("before-unsafe-get-parents")
      parents <- block.justifications
                  .traverse(BlockDagStorage[F].lookupUnsafe(_))
                  .map(_.filter(!_.validationFailed))
                  .map(_.map(_.blockHash))

      _          <- Span[F].mark("before-compute-parents-post-state")
      parentsSet = parents.toSet
      preState <- if (parentsSet.nonEmpty)
                   MultiParentCasper.getPreStateForParents(parents.toSet)
                 else {
                   // Genesis block
                   val genesisPreStateHash = RuntimeManager.emptyStateHashFixed.toBlake2b256Hash
                   ParentsMergedState(
                     justifications = Set[BlockMetadata](),
                     fringe = Set[BlockHash](),
                     fringeState = genesisPreStateHash,
                     // TODO: validate with data from bonds file
                     fringeBondsMap = block.bonds,
                     fringeRejectedDeploys = Set[ByteString](),
                     // TODO: validate with data from config (genesis block number)
                     maxBlockNum = 0L,
                     // TODO: validate with sender in bonds map
                     maxSeqNums = Map[Validator, Long](block.sender -> 0L),
                     // TODO: validate genesis post-state hash
                     preStateHash = genesisPreStateHash,
                     rejectedDeploys = Set()
                   ).pure[F]
                 }
      blockStr          = PrettyPrinter.buildString(block, short = true)
      fringeRejectedStr = PrettyPrinter.buildString(preState.fringeRejectedDeploys)
      rejectedStr       = PrettyPrinter.buildString(preState.rejectedDeploys)
      infoMsg           = s"Computed parents post state for block $blockStr, fringe rejections: $fringeRejectedStr, rejections: $rejectedStr"
      _                 <- Log[F].info(infoMsg)

      computedPreStateHash = preState.preStateHash.toByteString
      rejectedDeployIds    = preState.fringeRejectedDeploys
      result <- {
        val incomingPreStateHash = block.preStateHash
        if (incomingPreStateHash != computedPreStateHash) {
          //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
          Log[F]
            .warn(
              s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                .buildString(incomingPreStateHash)}"
            )
            .as(false.asRight[InvalidBlock])
        } else if (rejectedDeployIds != block.rejectedDeploys) {
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
          val rand = BlockRandomSeed.randomGenerator(block)
          for {
            replayResult <- replayBlock(incomingPreStateHash, block, rand)
            result       <- handleErrors(block.postStateHash, replayResult)
          } yield result.map(_.isDefined)
        }
      }
    } yield {
      val bmd = BlockMetadata
        .fromBlock(block)
        .copy(
          validated = true,
          validationFailed = result.isLeft || !result.right.get,
          fringe = preState.fringe,
          fringeStateHash = preState.fringeState.bytes.toArray.toByteString
        )
      (bmd, result)
    }

  def validateBlockCheckpointLegacy[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage
  ): F[BlockProcessing[Boolean]] = validateBlockCheckpoint(block).map(_._2)

  private def replayBlock[F[_]: Sync: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Span](
      initialStateHash: StateHash,
      block: BlockMessage,
      rand: Blake2b512Random
  ): F[Either[ReplayFailure, StateHash]] =
    Span[F].trace(ReplayBlockMetricsSource) {
      val internalDeploys       = block.state.deploys
      val internalSystemDeploys = block.state.systemDeploys
      for {
        _                  <- Span[F].mark("before-process-pre-state-hash")
        blockData          = BlockData.fromBlock(block)
        withCostAccounting = block.justifications.nonEmpty
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
              .as(none[StateHash].asRight[InvalidBlock])
          case UnusedCOMMEvent(replayException) =>
            Log[F]
              .warn(
                s"Found replay exception: ${replayException.getMessage}"
              )
              .as(none[StateHash].asRight[InvalidBlock])
          case ReplayCostMismatch(initialCost, replayCost) =>
            Log[F]
              .warn(
                s"Found replay cost mismatch: initial deploy cost = $initialCost, replay deploy cost = $replayCost"
              )
              .as(none[StateHash].asRight[InvalidBlock])
          // Restructure errors so that this case is unnecessary
          case SystemDeployErrorMismatch(playMsg, replayMsg) =>
            Log[F]
              .warn(
                s"Found system deploy error mismatch: initial deploy error message = $playMsg, replay deploy error message = $replayMsg"
              )
              .as(none[StateHash].asRight[InvalidBlock])
        }
      case Right(computedStateHash) =>
        if (tsHash == computedStateHash) {
          // state hash in block matches computed hash!
          computedStateHash.some.asRight[InvalidBlock].pure
        } else {
          // state hash in block does not match computed hash -- invalid!
          // return no state hash, do not update the state hash set
          Log[F]
            .warn(
              s"Tuplespace hash ${PrettyPrinter.buildString(tsHash)} does not match computed hash ${PrettyPrinter
                .buildString(computedStateHash)}."
            )
            .as(none[StateHash].asRight[InvalidBlock])
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
      preStateHash: StateHash
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    Span[F].trace(ComputeDeploysCheckpointMetricsSource) {
      for {
        result <- RuntimeManager[F].computeState(preStateHash)(
                   deploys,
                   systemDeploys,
                   rand,
                   blockData
                 )
        (postStateHash, processedDeploys, processedSystemDeploys) = result
      } yield (postStateHash, processedDeploys, processedSystemDeploys)
    }
}
