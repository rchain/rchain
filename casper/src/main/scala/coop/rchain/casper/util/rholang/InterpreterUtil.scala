package coop.rchain.casper.util.rholang

import cats.Monad
import cats.implicits._

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{BlockDag, BlockException, PrettyPrinter}
import coop.rchain.casper.PrettyPrinter.buildString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import cats.Id
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.{trace, Checkpoint}
import monix.execution.Scheduler
import scodec.Codec
import coop.rchain.shared.AttemptOps._
import scodec.bits.BitVector

import scala.collection.immutable

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Monad: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : F[(Either[BlockException, Option[StateHash]], Set[StateHash])] = {
    val tsHash          = ProtoUtil.tuplespace(b)
    val deploys         = ProtoUtil.deploys(b)
    val internalDeploys = deploys.flatMap(ProcessedDeployUtil.toInternal)
    for {
      parents                                    <- ProtoUtil.unsafeGetParents[F](b)
      cpps                                       <- computeParentsPostState[F](parents, genesis, dag, knownStateHashes, runtimeManager)
      (possiblePreStateHash, updatedStateHashes) = cpps
    } yield
      possiblePreStateHash match {
        case Left(ex) => Left(BlockException(ex)) -> knownStateHashes
        case Right(parentStateHash) =>
          runtimeManager.replayComputeState(parentStateHash, internalDeploys) match {
            case Left((deploy, status)) =>
              status match {
                case InternalErrors(exs) =>
                  Left(
                    BlockException(
                      new Exception(s"Internal errors encountered while processing ${PrettyPrinter
                        .buildString(deploy)}: ${exs.mkString("\n")}"))) -> knownStateHashes
                case _ => Right(None) -> knownStateHashes
              }

            case Right(computedStateHash) =>
              if (tsHash.contains(computedStateHash)) {
                // state hash in block matches computed hash!
                Right(Some(computedStateHash)) -> (updatedStateHashes + computedStateHash)
              } else {
                // state hash in block does not match computed hash -- invalid!
                // return no state hash, do not update the state hash set
                Right(None) -> knownStateHashes
              }
          }
      }
  }

  def computeDeploysCheckpoint[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      genesis: BlockMessage,
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

  private def computeParentsPostState[F[_]: Monad: BlockStore](parents: Seq[BlockMessage],
                                                               genesis: BlockMessage,
                                                               dag: BlockDag,
                                                               knownStateHashes: Set[StateHash],
                                                               runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): F[(Either[Throwable, StateHash], Set[StateHash])] = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

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

        gcaStateHash = ProtoUtil.tuplespace(gca).get
        _ = assert(
          knownStateHashes.contains(gcaStateHash),
          "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

        // TODO: Have proper merge of tuplespaces instead of recomputing.
        ancestors <- DagOperations
                      .bfTraverseF[F, BlockMessage](parentTuplespaces.map(_._1).toList) { block =>
                        if (block == gca) List.empty[BlockMessage].pure[F]
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
                 genesis,
                 dag,
                 knownStateHashes,
                 runtimeManager
               )
    } yield result
}
