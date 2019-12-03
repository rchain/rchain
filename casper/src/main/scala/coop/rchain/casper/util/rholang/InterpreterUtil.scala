package coop.rchain.casper.util.rholang

import cats.Monad
import cats.effect._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.NormalizerEnv.ToEnvMap
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockMetadata, NormalizerEnv, Par}
import coop.rchain.rholang.interpreter.ParBuilder
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.shared.{Log, LogSource}
import com.google.protobuf.ByteString
import coop.rchain.crypto.signatures.Signed
import monix.eval.Coeval

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def mkTerm[Env](rho: String, normalizerEnv: NormalizerEnv[Env])(
      implicit ev: ToEnvMap[Env]
  ): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, normalizerEnv.toEnv).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Sync: Log: BlockStore: Span](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = ProtoUtil.preStateHash(block)
    for {
      _                  <- Span[F].mark("before-unsafe-get-parents")
      parents            <- ProtoUtil.getParents(block)
      _                  <- Span[F].mark("before-compute-parents-post-state")
      preStateHashEither <- computeParentsPostState(parents, dag, runtimeManager).attempt
      _                  <- Log[F].info(s"Computed parents post state for ${PrettyPrinter.buildString(block)}.")
      result <- preStateHashEither match {
                 case Left(ex) =>
                   BlockStatus.exception(ex).asLeft[Option[StateHash]].pure
                 case Right(computedPreStateHash) =>
                   if (incomingPreStateHash == computedPreStateHash) {
                     replayBlockDeploys(block, dag, runtimeManager)
                   } else {
                     //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
                     Log[F]
                       .warn(
                         s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                           .buildString(incomingPreStateHash)}"
                       )
                       .as(none[StateHash].asRight[BlockError])
                   }
               }
    } yield result
  }

  //TODO deduplicate with `replayBlock` below - they are virtually identical!
  private def replayBlockDeploys[F[_]: Sync: Log: BlockStore: Span](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val preStateHash    = ProtoUtil.preStateHash(block)
    val internalDeploys = ProtoUtil.deploys(block)
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
      replayResult <- runtimeManager.replayComputeState(preStateHash)(
                       internalDeploys,
                       blockData,
                       invalidBlocks,
                       isGenesis
                     )
      tsHash = ProtoUtil.postStateHash(block)
      result <- handleErrors(tsHash, replayResult)
    } yield result
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

  def computeDeploysCheckpoint[F[_]: Sync: BlockStore: Log: Span](
      parents: Seq[BlockMessage],
      deploys: Seq[Signed[DeployData]],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] = {
    import shapeless.syntax.std.tuple._
    for {
      nonEmptyParents <- parents.pure
                          .ensure(new IllegalArgumentException("Parents must not be empty"))(
                            _.nonEmpty
                          )
      preStateHash <- computeParentsPostState(nonEmptyParents, dag, runtimeManager)
      result       <- runtimeManager.computeState(preStateHash)(deploys, blockData, invalidBlocks)
    } yield preStateHash +: result
  }

  private def computeParentsPostState[F[_]: Sync: BlockStore: Log: Span](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[StateHash] = {
    val parentTuplespaces =
      parents.map(p => p -> ProtoUtil.postStateHash(p))

    parentTuplespaces match {
      // For genesis, use empty trie's root hash
      case Seq() =>
        runtimeManager.emptyStateHash.pure

      case Seq((_, parentStateHash)) =>
        parentStateHash.pure

      case (_, initStateHash) +: _ =>
        replayIntoMergeBlock(parents, dag, runtimeManager, initStateHash)
    }
  }

  // In the case of multiple parents we need to apply all of the deploys that have been
  // made in all of the branches of the DAG being merged. This is done by computing uncommon ancestors
  // and applying the deploys in those blocks on top of the initial parent.
  private def replayIntoMergeBlock[F[_]: Sync: BlockStore: Log: Span](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      initStateHash: StateHash
  ): F[StateHash] = {
    import cats.instances.vector._
    for {
      _                  <- Span[F].mark("before-compute-parents-post-state-find-multi-parents")
      blockHashesToApply <- findMultiParentsBlockHashesForReplay(parents, dag)
      _ <- Log[F].info(
            s"replayIntoMergeBlock computed number of uncommon ancestors: ${blockHashesToApply.length}"
          )
      _             <- Span[F].mark("before-compute-parents-post-state-get-blocks")
      blocksToApply <- blockHashesToApply.traverse(b => ProtoUtil.getBlock(b.blockHash))
      _             <- Span[F].mark("before-compute-parents-post-state-replay")
      replayResult <- blocksToApply.foldM(initStateHash) {
                       replayBlock(_, _, parents, dag, runtimeManager)
                     }
    } yield replayResult
  }

  private[rholang] def replayBlock[F[_]: Sync: BlockStore](
      hash: StateHash,
      block: BlockMessage,
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[StateHash] = {
    val deploys   = block.body.deploys
    val isGenesis = parents.isEmpty

    (for {
      invalidBlocksSet <- dag.invalidBlocks
      unseenBlocksSet  <- ProtoUtil.unseenBlockHashes(dag, block)
      seenInvalidBlocksSet = invalidBlocksSet.filterNot(
        block => unseenBlocksSet.contains(block.blockHash)
      )
      invalidBlocks = seenInvalidBlocksSet
        .map(block => (block.blockHash, block.sender))
        .toMap
      replayResult <- runtimeManager.replayComputeState(hash)(
                       deploys,
                       BlockData.fromBlock(block),
                       invalidBlocks,
                       isGenesis //should always be false
                     )
    } yield replayResult.leftMap[Throwable] { status =>
      val parentHashes =
        parents.map(p => Base16.encode(p.blockHash.toByteArray).take(8))
      new Exception(
        s"Failed status while computing post state of $parentHashes: $status"
      )
    }).rethrow
  }

  private[rholang] def findMultiParentsBlockHashesForReplay[F[_]: Monad](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F]
  ): F[Vector[BlockMetadata]] = {
    import cats.instances.list._
    for {
      parentsMetadata <- parents.toList.traverse(b => dag.lookup(b.blockHash).map(_.get))
      blockHashesToApply <- {
        for {
          uncommonAncestors          <- DagOperations.uncommonAncestors(parentsMetadata.toVector, dag)
          ancestorsOfInitParentIndex = 0
          // Filter out blocks that already included by starting from the chosen initial parent
          // as otherwise we will be applying the initial parent's ancestor's twice.
          result = uncommonAncestors
            .filterNot { case (_, set) => set.contains(ancestorsOfInitParentIndex) }
            .keys
            .toVector
            .sorted(BlockMetadata.orderingByNum) // Ensure blocks to apply is topologically sorted to maintain any causal dependencies
        } yield result
      }
    } yield blockHashesToApply
  }
}
