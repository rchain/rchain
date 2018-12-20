package coop.rchain.casper.util.rholang

import cats.Monad
import cats.effect._
import cats.implicits._
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.catscontrib.ToAbstractContext
import coop.rchain.casper.{BlockException, PrettyPrinter}
import coop.rchain.casper.PrettyPrinter.buildString
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.{BlockException, PrettyPrinter}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rspace.ReplayException
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

object InterpreterUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def mkTerm(rho: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(rho).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Sync: Log: BlockStore: ToAbstractContext](
      b: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager
  ): F[Either[BlockException, Option[StateHash]]] = {
    val preStateHash    = ProtoUtil.preStateHash(b)
    val tsHash          = ProtoUtil.tuplespace(b)
    val deploys         = ProtoUtil.deploys(b)
    val internalDeploys = deploys.flatMap(ProcessedDeployUtil.toInternal)
    val timestamp       = Some(b.header.get.timestamp) // TODO: Ensure header exists through type
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      possiblePreStateHash <- computeParentsPostState[F](
                               parents,
                               dag,
                               runtimeManager,
                               timestamp
                             )
      result <- processPossiblePreStateHash[F](
                 runtimeManager,
                 preStateHash,
                 tsHash,
                 internalDeploys,
                 possiblePreStateHash,
                 timestamp
               )
    } yield result
  }

  private def processPossiblePreStateHash[F[_]: Sync: Log: BlockStore: ToAbstractContext](
      runtimeManager: RuntimeManager,
      preStateHash: StateHash,
      tsHash: Option[StateHash],
      internalDeploys: Seq[InternalProcessedDeploy],
      possiblePreStateHash: Either[Throwable, StateHash],
      time: Option[Long]
  ): F[Either[BlockException, Option[StateHash]]] =
    possiblePreStateHash match {
      case Left(ex) =>
        Left(BlockException(ex)).rightCast[Option[StateHash]].pure[F]
      case Right(computedPreStateHash) =>
        if (preStateHash == computedPreStateHash) {
          processPreStateHash[F](
            runtimeManager,
            preStateHash,
            tsHash,
            internalDeploys,
            possiblePreStateHash,
            time
          )
        } else {
          Log[F].warn(
            s"Computed pre-state hash ${PrettyPrinter.buildString(computedPreStateHash)} does not equal block's pre-state hash ${PrettyPrinter
              .buildString(preStateHash)}"
          ) *> Right(none[StateHash]).leftCast[BlockException].pure[F]
        }
    }

  private def processPreStateHash[F[_]: Sync: Log: BlockStore: ToAbstractContext](
      runtimeManager: RuntimeManager,
      preStateHash: StateHash,
      tsHash: Option[StateHash],
      internalDeploys: Seq[InternalProcessedDeploy],
      possiblePreStateHash: Either[Throwable, StateHash],
      time: Option[Long]
  ): F[Either[BlockException, Option[StateHash]]] =
    ToAbstractContext[F]
      .fromTask(runtimeManager.replayComputeState(preStateHash, internalDeploys, time))
      .flatMap {
        case Left((Some(deploy), status)) =>
          status match {
            case InternalErrors(exs) =>
              Left(
                BlockException(
                  new Exception(s"Internal errors encountered while processing ${PrettyPrinter
                    .buildString(deploy)}: ${exs.mkString("\n")}")
                )
              ).rightCast[Option[StateHash]].pure[F]
            case UserErrors(errors: Vector[Throwable]) =>
              Log[F].warn(s"Found user error(s) ${errors.map(_.getMessage).mkString("\n")}") *> Right(
                none[StateHash]
              ).leftCast[BlockException].pure[F]
            case ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) =>
              Log[F].warn(
                s"Found replay status mismatch; replay failure is ${replay.isFailed} and orig failure is ${orig.isFailed}"
              ) *> Right(none[StateHash]).leftCast[BlockException].pure[F]
            case UnknownFailure =>
              Log[F].warn(s"Found unknown failure") *> Right(none[StateHash])
                .leftCast[BlockException]
                .pure[F]
          }
        case Left((None, status)) =>
          status match {
            case UnusedCommEvent(ex: ReplayException) =>
              Log[F].warn(s"Found unused comm event ${ex.getMessage}") *> Right(none[StateHash])
                .leftCast[BlockException]
                .pure[F]
          }
        case Right(computedStateHash) =>
          if (tsHash.contains(computedStateHash)) {
            // state hash in block matches computed hash!
            Right(Option(computedStateHash)).leftCast[BlockException].pure[F]
          } else {
            // state hash in block does not match computed hash -- invalid!
            // return no state hash, do not update the state hash set
            Log[F].warn(
              s"Tuplespace hash ${tsHash.getOrElse(ByteString.EMPTY)} does not match computed hash $computedStateHash."
            ) *> Right(none[StateHash]).leftCast[BlockException].pure[F]

          }
      }

  def computeDeploysCheckpoint[F[_]: Sync: BlockStore: ToAbstractContext](
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager,
      time: Option[Long] = None
  )(
      implicit scheduler: Scheduler
  ): F[Either[Throwable, (StateHash, StateHash, Seq[InternalProcessedDeploy])]] =
    for {
      possiblePreStateHash <- computeParentsPostState[F](parents, dag, runtimeManager, time)
    } yield
      possiblePreStateHash match {
        case Right(preStateHash) =>
          val (postStateHash, processedDeploys) =
            runtimeManager.computeState(preStateHash, deploys, time).runSyncUnsafe(Duration.Inf)
          Right(preStateHash, postStateHash, processedDeploys)
        case Left(err) =>
          Left(err)
      }

  private def computeParentsPostState[F[_]: Sync: BlockStore: ToAbstractContext](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager,
      time: Option[Long]
  ): F[Either[Throwable, StateHash]] = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    parentTuplespaces match {
      // For genesis, use empty trie's root hash
      case Seq() =>
        Right(runtimeManager.emptyStateHash).leftCast[Throwable].pure[F]

      case Seq((_, parentStateHash)) =>
        Right(parentStateHash).leftCast[Throwable].pure[F]

      case (_, initStateHash) +: _ =>
        computeMultiParentsPostState[F](
          parents,
          dag,
          runtimeManager,
          time,
          initStateHash
        )
    }
  }
  // In the case of multiple parents we need to apply all of the deploys that have been
  // made in all of the branches of the DAG being merged. This is done by computing uncommon ancestors
  // and applying the deploys in those blocks on top of the initial parent.
  private def computeMultiParentsPostState[F[_]: Sync: BlockStore: ToAbstractContext](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager,
      time: Option[Long],
      initStateHash: StateHash
  ): F[Either[Throwable, StateHash]] =
    for {
      blockHashesToApply <- findMultiParentsBlockHashesForReplay(parents, dag)
      blocksToApply      <- blockHashesToApply.traverse(b => ProtoUtil.unsafeGetBlock[F](b.blockHash))
      deploys            = blocksToApply.flatMap(_.getBody.deploys.flatMap(ProcessedDeployUtil.toInternal))
      replayResult <- ToAbstractContext[F].fromTask(
                       runtimeManager
                         .replayComputeState(initStateHash, deploys, time)
                     )
    } yield
      replayResult match {
        case result @ Right(_) => result.leftCast[Throwable]
        case Left((_, status)) =>
          val parentHashes =
            parents.map(p => Base16.encode(p.blockHash.toByteArray).take(8))
          Left(
            new Exception(
              s"Failed status while computing post state of $parentHashes: $status"
            )
          )
      }

  private[rholang] def findMultiParentsBlockHashesForReplay[F[_]: Monad](
      parents: Seq[BlockMessage],
      dag: BlockDagRepresentation[F]
  ): F[Vector[BlockMetadata]] =
    for {
      parentsMetadata <- parents.toList.traverse(b => dag.lookup(b.blockHash).map(_.get))
      ordering        <- dag.deriveOrdering(0L) // TODO: Replace with an actual starting number
      blockHashesToApply <- {
        implicit val o: Ordering[BlockMetadata] = ordering
        for {
          uncommonAncestors          <- DagOperations.uncommonAncestors[F](parentsMetadata.toVector, dag)
          ancestorsOfInitParentIndex = 0
          // Filter out blocks that already included by starting from the chosen initial parent
          // as otherwise we will be applying the initial parent's ancestor's twice.
          result = uncommonAncestors
            .filterNot { case (_, set) => set.contains(ancestorsOfInitParentIndex) }
            .keys
            .toVector
            .sorted // Ensure blocks to apply is topologically sorted to maintain any causal dependencies
        } yield result
      }
    } yield blockHashesToApply

  private[casper] def computeBlockCheckpointFromDeploys[F[_]: Sync: BlockStore: ToAbstractContext](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager
  )(
      implicit scheduler: Scheduler
  ): F[Either[Throwable, (StateHash, StateHash, Seq[InternalProcessedDeploy])]] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)

      deploys = ProtoUtil.deploys(b).flatMap(_.deploy)

      _ = assert(
        parents.nonEmpty || (parents.isEmpty && b == genesis),
        "Received a different genesis block."
      )

      result <- computeDeploysCheckpoint[F](
                 parents,
                 deploys,
                 dag,
                 runtimeManager
               )
    } yield result
}
