package coop.rchain.casper.util.rholang

import cats.Monad
import cats.effect._
import cats.syntax.all._

import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.rholang.RuntimeManager._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Span
import coop.rchain.models.{BlockMetadata, Par}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.{NormalizerEnv, ParBuilder}
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rspace.ReplayException
import coop.rchain.shared.{Log, LogSource}

import com.google.protobuf.ByteString
import monix.eval.Coeval

object InterpreterUtil {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def mkTerm(rho: String, normalizerEnv: NormalizerEnv): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, normalizerEnv).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Sync: Log: BlockStore: Span](
      b: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[BlockProcessing[Option[StateHash]]] = {
    val incomingPreStateHash = ProtoUtil.preStateHash(b)
    val tsHash               = ProtoUtil.tuplespace(b)
    val deploys              = ProtoUtil.deploys(b)
    val internalDeploys      = deploys.map(InternalProcessedDeploy.fromProcessedDeploy)
    val timestamp            = b.header.timestamp
    val blockNumber          = b.body.state.blockNumber
    for {
      _                    <- Span[F].mark("before-unsafe-get-parents")
      parents              <- ProtoUtil.getParents(b)
      _                    <- Span[F].mark("before-compute-parents-post-state")
      possiblePreStateHash <- computeParentsPostState(parents, dag, runtimeManager).attempt
      _                    <- Log[F].info(s"Computed parents post state for ${PrettyPrinter.buildString(b)}.")
      invalidBlocksSet     <- dag.invalidBlocks
      unseenBlocksSet      <- ProtoUtil.unseenBlockHashes(dag, b)
      seenInvalidBlocksSet = invalidBlocksSet.filterNot(
        block => unseenBlocksSet.contains(block.blockHash)
      ) // TODO: Write test in which switching this to .filter makes it fail
      invalidBlocks = seenInvalidBlocksSet.map(block => (block.blockHash, block.sender)).toMap
      _             <- Span[F].mark("before-process-pre-state-hash")
      isGenesis     = b.header.parentsHashList.isEmpty
      result <- possiblePreStateHash match {
                 case Left(ex) =>
                   BlockStatus.exception(ex).asLeft[Option[StateHash]].pure
                 case Right(computedPreStateHash) =>
                   if (incomingPreStateHash == computedPreStateHash) {
                     replayBlockDeploys(
                       runtimeManager,
                       incomingPreStateHash,
                       tsHash,
                       internalDeploys,
                       BlockData(timestamp, blockNumber),
                       invalidBlocks,
                       isGenesis
                     )
                   } else {
                     //TODO at this point we may just as well terminate the replay, there's no way it will succeed.
                     Log[F].warn(
                       s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
                         .buildString(incomingPreStateHash)}"
                     ) >> none[StateHash].asRight[BlockError].pure
                   }
               }
    } yield result
  }

  private def replayBlockDeploys[F[_]: Sync: Log: BlockStore](
      runtimeManager: RuntimeManager[F],
      preStateHash: StateHash,
      tsHash: StateHash,
      internalDeploys: Seq[InternalProcessedDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator],
      isGenesis: Boolean
  ): F[BlockProcessing[Option[StateHash]]] =
    runtimeManager
      .replayComputeState(preStateHash)(internalDeploys, blockData, invalidBlocks, isGenesis)
      .flatMap {
        case Left((Some(deploy), status)) =>
          status match {
            case InternalErrors(exs) =>
              BlockStatus
                .exception(
                  new Exception(s"Internal errors encountered while processing ${PrettyPrinter
                    .buildString(deploy)}: ${exs.mkString("\n")}")
                )
                .asLeft[Option[StateHash]]
                .pure
            case UserErrors(errors: Seq[Throwable]) =>
              Log[F].warn(s"Found user error(s) ${errors.map(_.getMessage).mkString("\n")}") >>
                none[StateHash].asRight[BlockError].pure
            case ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) =>
              Log[F].warn(
                s"Found replay status mismatch; replay failure is ${replay.isFailed} and orig failure is ${orig.isFailed}"
              ) >>
                none[StateHash].asRight[BlockError].pure
            case UnknownFailure =>
              Log[F].warn(s"Found unknown failure") >>
                none[StateHash].asRight[BlockError].pure
            case UnusedCommEvent(_) =>
              Sync[F].raiseError(new RuntimeException("found UnusedCommEvent"))
          }
        case Left((None, status)) =>
          status match {
            case UnusedCommEvent(_: ReplayException) =>
              none[StateHash].asRight[BlockError].pure
            case InternalErrors(_) => new RuntimeException("found InternalErrors").raiseError
            case ReplayStatusMismatch(_, _) =>
              new RuntimeException("found ReplayStatusMismatch").raiseError
            case UnknownFailure => new RuntimeException("found UnknownFailure").raiseError
            case UserErrors(_)  => new RuntimeException("found UserErrors").raiseError
          }
        case Right(computedStateHash) =>
          if (tsHash == computedStateHash) {
            // state hash in block matches computed hash!
            computedStateHash.some.asRight[BlockError].pure
          } else {
            // state hash in block does not match computed hash -- invalid!
            // return no state hash, do not update the state hash set
            Log[F].warn(
              s"Tuplespace hash ${PrettyPrinter.buildString(tsHash)} does not match computed hash ${PrettyPrinter
                .buildString(computedStateHash)}."
            ) >>
              none[StateHash].asRight[BlockError].pure

          }
      }

  def computeDeploysCheckpoint[F[_]: Sync: BlockStore: Log: Span](
      parents: Seq[BlockMessage],
      deploys: Seq[DeployData],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator]
  ): F[Either[Throwable, (StateHash, StateHash, Seq[InternalProcessedDeploy])]] = {
    import cats.instances.either._
    for {
      nonEmptyParents <- parents
                          .pure[F]
                          .ensure(new IllegalArgumentException("Parents must not be empty"))(
                            _.nonEmpty
                          )
      possiblePreStateHash <- computeParentsPostState(nonEmptyParents, dag, runtimeManager).attempt
      result <- possiblePreStateHash.flatTraverse { preStateHash =>
                 runtimeManager
                   .computeState(preStateHash)(deploys, blockData, invalidBlocks)
                   .map {
                     case (postStateHash, processedDeploys) =>
                       (preStateHash, postStateHash, processedDeploys).asRight[Throwable]
                   }
               }
    } yield result
  }

  private def computeParentsPostState[F[_]: Sync: BlockStore: Log: Span](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[StateHash] = {
    val parentTuplespaces =
      parents.map(p => p -> ProtoUtil.tuplespace(p))

    parentTuplespaces match {
      // For genesis, use empty trie's root hash
      case Seq() =>
        runtimeManager.emptyStateHash.pure

      case Seq((_, parentStateHash)) =>
        parentStateHash.pure

      case (_, initStateHash) +: _ =>
        replayIntoMergeBlock(parents, dag, runtimeManager, initStateHash).rethrow
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
  ): F[Either[Throwable, StateHash]] = {
    import cats.instances.vector._
    for {
      _                  <- Span[F].mark("before-compute-parents-post-state-find-multi-parents")
      blockHashesToApply <- findMultiParentsBlockHashesForReplay(parents, dag)
      _ <- Log[F].info(
            s"replayIntoMergeBlock computed number of parents: ${blockHashesToApply.length}"
          )
      _             <- Span[F].mark("before-compute-parents-post-state-get-blocks")
      blocksToApply <- blockHashesToApply.traverse(b => ProtoUtil.getBlock(b.blockHash))
      _             <- Span[F].mark("before-compute-parents-post-state-replay")
      replayResult <- (initStateHash, blocksToApply).tailRecM {
                       case (hash, blocks) if blocks.isEmpty =>
                         hash.asRight[Throwable].asRight[(StateHash, Vector[BlockMessage])].pure[F]

                       case (hash, blocks) =>
                         replayBlock(hash, blocks.head, parents.isEmpty, dag, runtimeManager).map {
                           case Right(h) => (h, blocks.tail).asLeft
                           case Left((_, status)) =>
                             val parentHashes =
                               parents.map(p => Base16.encode(p.blockHash.toByteArray).take(8))
                             new Exception(
                               s"Failed status while computing post state of $parentHashes: $status"
                             ).asInstanceOf[Throwable]
                               .asLeft[StateHash]
                               .asRight
                         }
                     }
    } yield replayResult
  }

  private[rholang] def replayBlock[F[_]: Sync: BlockStore](
      hash: StateHash,
      block: BlockMessage,
      isGenesis: Boolean,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F]
  ): F[Either[ReplayFailure, StateHash]] = {
    val deploys     = block.body.deploys.map(InternalProcessedDeploy.fromProcessedDeploy)
    val timestamp   = block.header.timestamp
    val blockNumber = block.body.state.blockNumber

    for {
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
                       BlockData(timestamp, blockNumber),
                       invalidBlocks,
                       isGenesis //should always be false
                     )
    } yield replayResult
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
