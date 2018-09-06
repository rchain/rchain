package coop.rchain.casper.util.rholang

import cats.Monad
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{BlockDag, BlockException, PrettyPrinter}
import coop.rchain.casper.PrettyPrinter.buildString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.ReplayException
import monix.execution.Scheduler
import coop.rchain.shared.{Log, LogSource}

import scala.collection.immutable

object InterpreterUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Monad: Log: BlockStore](
      b: BlockMessage.BlockMessageSafe,
      genesis: BlockMessage.BlockMessageSafe,
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : F[(Either[BlockException, Option[StateHash]], Set[StateHash])] = {
    val tsHash          = ProtoUtil.tuplespace(b)
    val deploys         = ProtoUtil.deploys(b)
    val internalDeploys = deploys.flatMap(ProcessedDeployUtil.toInternal)
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      parentsPostStateResult <- computeParentsPostState[F](parents,
                                                           genesis,
                                                           dag,
                                                           knownStateHashes,
                                                           runtimeManager)
      (possiblePreStateHash, updatedStateHashes) = parentsPostStateResult
      result <- processPossiblePreStateHash[F](knownStateHashes,
                                               runtimeManager,
                                               tsHash,
                                               internalDeploys,
                                               possiblePreStateHash,
                                               updatedStateHashes)
    } yield result

  }

  private def processPossiblePreStateHash[F[_]: Monad: Log: BlockStore](
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager,
      tsHash: StateHash,
      internalDeploys: Seq[InternalProcessedDeploy],
      possiblePreStateHash: Either[Throwable, StateHash],
      updatedStateHashes: Set[StateHash])(implicit scheduler: Scheduler)
    : F[(Either[BlockException, Option[StateHash]], Set[StateHash])] =
    possiblePreStateHash match {
      case Left(ex) =>
        (Left(BlockException(ex)).rightCast[Option[StateHash]] -> knownStateHashes).pure[F]
      case Right(parentStateHash) =>
        runtimeManager.replayComputeState(parentStateHash, internalDeploys) match {
          case Left((Some(deploy), status)) =>
            status match {
              case InternalErrors(exs) =>
                (Left(
                  BlockException(
                    new Exception(s"Internal errors encountered while processing ${PrettyPrinter
                      .buildString(deploy)}: ${exs.mkString("\n")}")))
                  .rightCast[Option[StateHash]] -> knownStateHashes).pure[F]
              case UserErrors(errors: Vector[Throwable]) =>
                Log[F].warn(s"Found user error(s) ${errors.map(_.getMessage).mkString("\n")}") *> (Right(
                  none[StateHash]).leftCast[BlockException] -> knownStateHashes).pure[F]
              case ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) =>
                Log[F].warn(
                  s"Found replay status mismatch; replay failure is $replay.isFailed and orig failure is $orig.isFailed") *> (Right(
                  none[StateHash]).leftCast[BlockException] -> knownStateHashes).pure[F]
              case UnknownFailure =>
                Log[F].warn(s"Found unknown failure") *> (Right(none[StateHash])
                  .leftCast[BlockException] -> knownStateHashes).pure[F]
            }
          case Left((None, status)) =>
            status match {
              case UnusedCommEvent(ex: ReplayException) =>
                Log[F].warn(s"Found unused comm event ${ex.getMessage}") *> (Right(none[StateHash])
                  .leftCast[BlockException] -> knownStateHashes).pure[F]
            }
          case Right(computedStateHash) =>
            if (tsHash == computedStateHash) {
              // state hash in block matches computed hash!
              (Right(Option(computedStateHash))
                .leftCast[BlockException] -> (updatedStateHashes + computedStateHash)).pure[F]
            } else {
              // state hash in block does not match computed hash -- invalid!
              // return no state hash, do not update the state hash set
              Log[F].warn(
                s"Tuplespace hash $tsHash does not match computed hash $computedStateHash.") *> (Right(
                none[StateHash]).leftCast[BlockException] -> knownStateHashes).pure[F]
            }
        }
    }
  def computeDeploysCheckpoint[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage.BlockMessageSafe],
      deploys: Seq[Deploy],
      genesis: BlockMessage.BlockMessageSafe,
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : F[(Either[Throwable, (StateHash, Seq[InternalProcessedDeploy])], Set[StateHash])] =
    for {
      cpps                                       <- computeParentsPostState[F](parents, genesis, dag, knownStateHashes, runtimeManager)
      (possiblePreStateHash, updatedStateHashes) = cpps
    } yield
      possiblePreStateHash match {
        case Right(preStateHash) =>
          val (postStateHash, processedDeploys) = runtimeManager.computeState(preStateHash, deploys)
          Right(postStateHash, processedDeploys) -> (updatedStateHashes + postStateHash)

        case Left(err) =>
          Left(err) -> updatedStateHashes
      }

  private def computeParentsPostState[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage.BlockMessageSafe],
      genesis: BlockMessage.BlockMessageSafe,
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): F[(Either[Throwable, StateHash], Set[StateHash])] = {
    val parentTuplespaces = parents.map(p => p -> ProtoUtil.tuplespace(p))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (Right(runtimeManager.emptyStateHash).leftCast[Throwable], knownStateHashes).pure[F]
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      assert(
        knownStateHashes.contains(parentStateHash),
        s"We should have already computed parent state hash when we added the parent tuplespace hash ${buildString(parentStateHash)} to our blockDAG."
      )
      (Right(parentStateHash).leftCast[Throwable], knownStateHashes).pure[F]
    } else {
      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all histories since the greatest
      //common ancestor in order to reach the current
      //state.
      for {
        gca <- parents.toList.foldM(parents.head) {
                case (gca, parent) =>
                  DagOperations.greatestCommonAncestorF[F](gca, parent, genesis, dag)
              }

        gcaStateHash = ProtoUtil.tuplespace(gca)
        _ = assert(
          knownStateHashes.contains(gcaStateHash),
          "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

        // TODO: Have proper merge of tuplespaces instead of recomputing.
        ancestors <- DagOperations
                      .bfTraverseF[F, BlockMessage.BlockMessageSafe](
                        parentTuplespaces.map(_._1).toList) { block =>
                        if (block == gca) List.empty[BlockMessage.BlockMessageSafe].pure[F]
                        else ProtoUtil.unsafeGetParents[F](block)
                      }
                      .filter(_ != gca) //do not include gca deploys
                      .toList

        deploys = ancestors
          .flatMap(ProtoUtil.deploys(_).reverse.flatMap(ProcessedDeployUtil.toInternal))
          .toIndexedSeq
          .reverse

      } yield
        runtimeManager.replayComputeState(gcaStateHash, deploys) match {
          case result @ Right(hash) => result.leftCast[Throwable] -> (knownStateHashes + hash)
          case Left((_, status)) =>
            Left(new Exception(s"Failed status while computing parent post state: $status")) -> knownStateHashes
        }
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys[F[_]: Monad: BlockStore](
      b: BlockMessage.BlockMessageSafe,
      genesis: BlockMessage.BlockMessageSafe,
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : F[(Either[Throwable, (StateHash, Seq[InternalProcessedDeploy])], Set[StateHash])] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)

      deploys = ProtoUtil.deploys(b).flatMap(_.deploy)

      _ = assert(parents.nonEmpty || (parents.isEmpty && b == genesis),
                 "Received a different genesis block.")

      result <- computeDeploysCheckpoint[F](
                 parents,
                 deploys,
                 genesis,
                 dag,
                 knownStateHashes,
                 runtimeManager
               )
    } yield result
}
