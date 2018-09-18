package coop.rchain.casper.util.rholang

import cats.Monad
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{BlockDag, BlockException, PrettyPrinter}
import coop.rchain.casper.PrettyPrinter.buildString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.crypto.codec.Base16
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
      b: BlockMessage,
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
      tsHash: Option[StateHash],
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
            if (tsHash.contains(computedStateHash)) {
              // state hash in block matches computed hash!
              (Right(Option(computedStateHash))
                .leftCast[BlockException] -> (updatedStateHashes + computedStateHash)).pure[F]
            } else {
              // state hash in block does not match computed hash -- invalid!
              // return no state hash, do not update the state hash set
              Log[F].warn(
                s"Tuplespace hash ${tsHash.getOrElse(ByteString.EMPTY)} does not match computed hash $computedStateHash.") *> (Right(
                none[StateHash]).leftCast[BlockException] -> knownStateHashes).pure[F]
            }
        }
    }
  def computeDeploysCheckpoint[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : F[(Either[Throwable, (StateHash, Seq[InternalProcessedDeploy])], Set[StateHash])] =
    for {
      cpps                                       <- computeParentsPostState[F](parents, dag, knownStateHashes, runtimeManager)
      (possiblePreStateHash, updatedStateHashes) = cpps
    } yield
      possiblePreStateHash match {
        case Right(preStateHash) =>
          val (postStateHash, processedDeploys) = runtimeManager.computeState(preStateHash, deploys)
          Right(postStateHash, processedDeploys) -> (updatedStateHashes + postStateHash)

        case Left(err) =>
          Left(err) -> updatedStateHashes
      }

  private def computeParentsPostState[F[_]: Monad: BlockStore](parents: Seq[BlockMessage],
                                                               dag: BlockDag,
                                                               knownStateHashes: Set[StateHash],
                                                               runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): F[(Either[Throwable, StateHash], Set[StateHash])] = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    parentTuplespaces match {
      //no parents to base off of, so use default
      case Seq() =>
        (Right(runtimeManager.emptyStateHash).leftCast[Throwable], knownStateHashes).pure[F]

      //For a single parent we look up its checkpoint
      case Seq((_, parentStateHash)) =>
        assert(
          knownStateHashes.contains(parentStateHash),
          "We should have already computed parent state hash when we added " +
            s"the parent tuplespace hash ${buildString(parentStateHash)} to our blockDAG."
        )
        (Right(parentStateHash).leftCast[Throwable], knownStateHashes).pure[F]

      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all of the branches of the DAG being
      //merged. This is done by computing uncommon ancestors
      //and applying the deploys in those blocks.
      case (initParent, initStateHash) +: _ =>
        implicit val ordering = BlockDag.deriveOrdering(dag)
        val indexedParents    = parents.toVector.map(b => dag.dataLookup(b.blockHash))
        val uncommonAncestors = DagOperations.uncommonAncestors(indexedParents, dag.dataLookup)
        assert(
          knownStateHashes.contains(initStateHash),
          "We should have already computed parent state hash when we added " +
            s"the parent tuplespace hash ${buildString(initStateHash)} to our blockDAG."
        )

        val initIndex = indexedParents.indexOf(dag.dataLookup(initParent.blockHash))
        //filter out blocks that already included by starting from the chosen initParent
        val blocksToApply = uncommonAncestors
          .filterNot { case (_, set) => set.contains(initIndex) }
          .keys
          .toVector
          .sorted //ensure blocks to apply is topologically sorted to maintain any causal dependencies

        for {
          blocks  <- blocksToApply.traverse(b => ProtoUtil.unsafeGetBlock[F](b.blockHash))
          deploys = blocks.flatMap(_.getBody.deploys.flatMap(ProcessedDeployUtil.toInternal))
        } yield
          runtimeManager.replayComputeState(initStateHash, deploys) match {
            case result @ Right(hash) => result.leftCast[Throwable] -> (knownStateHashes + hash)
            case Left((_, status)) =>
              val parentHashes = parents.map(p => Base16.encode(p.blockHash.toByteArray).take(8))
              Left(
                new Exception(
                  s"Failed status while computing post state of $parentHashes: $status")) -> knownStateHashes
          }
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys[F[_]: Monad: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
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
                 dag,
                 knownStateHashes,
                 runtimeManager
               )
    } yield result
}
