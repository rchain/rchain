package coop.rchain.casper.util.rholang

import coop.rchain.casper.BlockDag
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

import ProcessedDeployUtil.InternalProcessedDeploy

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint(b: BlockMessage,
                              genesis: BlockMessage,
                              dag: BlockDag,
                              internalMap: Map[BlockHash, BlockMessage],
                              knownStateHashes: Set[StateHash],
                              runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (Option[StateHash], Set[StateHash]) = {
    val tsHash          = ProtoUtil.tuplespace(b)
    val deploys         = ProtoUtil.deploys(b)
    val internalDeploys = deploys.flatMap(ProcessedDeployUtil.toInternal)
    val parents         = ProtoUtil.parents(b).map(internalMap)
    val (possiblePreStateHash, updatedStateHashes) =
      computeParentsPostState(parents, genesis, dag, internalMap, knownStateHashes, runtimeManager)

    possiblePreStateHash.flatMap(runtimeManager.replayComputeState(_, internalDeploys)) match {
      //TODO Log errors somewhere?
      case Left(_) => None -> knownStateHashes

      case Right(computedStateHash) =>
        if (tsHash.contains(computedStateHash)) {
          // state hash in block matches computed hash!
          Some(computedStateHash) -> updatedStateHashes
        } else {
          // state hash in block does not match computed hash -- invalid!
          // return no state hash, do not update the state hash set
          None -> knownStateHashes
        }
    }
  }

  def computeDeploysCheckpoint(parents: Seq[BlockMessage],
                               deploys: Seq[Deploy],
                               genesis: BlockMessage,
                               dag: BlockDag,
                               internalMap: Map[BlockHash, BlockMessage],
                               knownStateHashes: Set[StateHash],
                               runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : (Either[Throwable, Seq[InternalProcessedDeploy]], Set[StateHash]) = {
    val (possiblePreStateHash, updatedStateHashes) =
      computeParentsPostState(parents, genesis, dag, internalMap, knownStateHashes, runtimeManager)

    possiblePreStateHash match {
      case Right(preStateHash) =>
        val (postStateHash, processedDeploys) = runtimeManager.computeState(preStateHash, deploys)
        Right(processedDeploys) -> (updatedStateHashes + postStateHash)

      case Left(err) =>
        Left(err) -> updatedStateHashes
    }
  }

  private def computeParentsPostState(parents: Seq[BlockMessage],
                                      genesis: BlockMessage,
                                      dag: BlockDag,
                                      internalMap: Map[BlockHash, BlockMessage],
                                      knownStateHashes: Set[StateHash],
                                      runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (Either[Throwable, StateHash], Set[StateHash]) = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (Right(runtimeManager.emptyStateHash), knownStateHashes)
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      assert(
        knownStateHashes.contains(parentStateHash),
        "We should have already computed parent state hash when we added the parent to our blockDAG.")
      (Right(parentStateHash), knownStateHashes)
    } else {
      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all histories since the greatest
      //common ancestor in order to reach the current
      //state.
      val gca =
        parentTuplespaces
          .map(_._1)
          .reduce(DagOperations.greatestCommonAncestor(_, _, genesis, dag, internalMap))

      val gcaStateHash = ProtoUtil.tuplespace(gca).get
      assert(
        knownStateHashes.contains(gcaStateHash),
        "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

      // TODO: Have proper merge of tuplespaces instead of recomputing.
      // TODO: Fix so that all search branches reach GCA before quitting
      val deploys = DagOperations
        .bfTraverse[BlockMessage](parentTuplespaces.map(_._1))(
          ProtoUtil.parents(_).iterator.map(internalMap.apply))
        .takeWhile(_ != gca)
        .flatMap(ProtoUtil.deploys(_).reverse)
        .toIndexedSeq
        .reverse
        .flatMap(ProcessedDeployUtil.toInternal)

      runtimeManager.replayComputeState(gcaStateHash, deploys) match {
        case result @ Right(hash) => result -> (knownStateHashes + hash)
        case Left(ex) =>
          Left(new Exception(s"Error computing parent post state: ${ex.getMessage}")) -> knownStateHashes
      }
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      internalMap: Map[BlockHash, BlockMessage],
      knownStateHashes: Set[StateHash],
      runtimeManager: RuntimeManager)(implicit scheduler: Scheduler)
    : (Either[Throwable, Seq[InternalProcessedDeploy]], Set[StateHash]) = {
    val parents = ProtoUtil
      .parents(b)
      .map(internalMap.apply)

    val deploys = ProtoUtil.deploys(b).flatMap(_.deploy)

    assert(parents.nonEmpty || (parents.isEmpty && b == genesis),
           "Received a different genesis block.")

    computeDeploysCheckpoint(
      parents,
      deploys,
      genesis,
      dag,
      internalMap,
      knownStateHashes,
      runtimeManager
    )
  }
}
