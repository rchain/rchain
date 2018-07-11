package coop.rchain.casper.util.rholang

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.trace.Event
import coop.rchain.rspace.trace.Event._
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
  def validateBlockCheckpoint(b: BlockMessage,
                              genesis: BlockMessage,
                              dag: BlockDag,
                              defaultStateHash: StateHash,
                              knownStateHashes: Set[StateHash],
                              runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (Option[StateHash], Set[StateHash]) = {
    val tsHash        = ProtoUtil.tuplespace(b)
    val serializedLog = b.body.get.commReductions
    val log           = serializedLog.map(EventConverter.toRspaceEvent).toList
    val (computedCheckpoint, updatedStateHashes) =
      computeBlockCheckpointFromDeploys(b,
                                        genesis,
                                        dag,
                                        defaultStateHash,
                                        knownStateHashes,
                                        runtimeManager.replayComputeState(log))
    val computedStateHash = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
    if (tsHash.contains(computedStateHash)) {
      // state hash in block matches computed hash!
      Some(computedStateHash) -> updatedStateHashes
    } else {
      // state hash in block does not match computed hash -- invalid!
      // return no state hash, do not update the state hash set
      None -> knownStateHashes
    }
  }

  def computeDeploysCheckpoint(
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      genesis: BlockMessage,
      dag: BlockDag,
      defaultStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash, Seq[Deploy]) => Either[Throwable, Checkpoint])(
      implicit scheduler: Scheduler): (Checkpoint, Set[StateHash]) = {
    val (postStateHash, updatedStateHashes) =
      computeParentsPostState(parents,
                              genesis,
                              dag,
                              defaultStateHash,
                              knownStateHashes,
                              computeState)

    val Right(postDeploysCheckpoint) = computeState(postStateHash, deploys)
    val postDeploysStateHash         = ByteString.copyFrom(postDeploysCheckpoint.root.bytes.toArray)
    (postDeploysCheckpoint, updatedStateHashes + postDeploysStateHash)
  }

  private[casper] def computeBlockCheckpoint(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      defaultStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash, Seq[Deploy]) => Either[Throwable, Checkpoint])(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {

    val blockStateHash = ProtoUtil.tuplespace(b).get
    if (knownStateHashes.contains(blockStateHash)) {
      (blockStateHash, knownStateHashes)
    } else {
      val (checkpoint, updatedKnownStateHashes) = computeBlockCheckpointFromDeploys(
        b,
        genesis,
        dag,
        defaultStateHash,
        knownStateHashes,
        computeState)
      val blockStateHash = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      (blockStateHash, updatedKnownStateHashes)
    }
  }

  private def computeParentsPostState(
      parents: Seq[BlockMessage],
      genesis: BlockMessage,
      dag: BlockDag,
      defaultStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash, Seq[Deploy]) => Either[Throwable, Checkpoint])(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (defaultStateHash, knownStateHashes)
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      assert(
        knownStateHashes.contains(parentStateHash),
        "We should have already computed parent state hash when we added the parent to our blockDAG.")
      (parentStateHash, knownStateHashes)
    } else {
      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all histories since the greatest
      //common ancestor in order to reach the current
      //state.
      val gca =
        parentTuplespaces
          .map(_._1)
          .reduce(DagOperations.greatestCommonAncestor(_, _, genesis, dag))

      val gcaStateHash = ProtoUtil.tuplespace(gca).get
      assert(
        knownStateHashes.contains(gcaStateHash),
        "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

      // TODO: Fix so that all search branches reach GCA before quitting
      val deploys = DagOperations
        .bfTraverse[BlockMessage](parentTuplespaces.map(_._1))(
          ProtoUtil.parents(_).iterator.map(dag.blockLookup))
        .takeWhile(_ != gca)
        .flatMap(ProtoUtil.deploys(_).reverse)
        .toIndexedSeq
        .reverse

      //TODO: figure out what casper should do with errors in deploys
      val Right(resultStateCheckpoint) =
        computeState(gcaStateHash, deploys)
      val resultStateHash = ByteString.copyFrom(resultStateCheckpoint.root.bytes.toArray)
      (resultStateHash, knownStateHashes + resultStateHash)
    }
  }

  private def computeBlockCheckpointFromDeploys(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      defaultStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash, Seq[Deploy]) => Either[Throwable, Checkpoint])(
      implicit scheduler: Scheduler): (Checkpoint, Set[StateHash]) = {
    val parents = ProtoUtil
      .parents(b)
      .map(dag.blockLookup)

    val deploys = ProtoUtil.deploys(b)

    computeDeploysCheckpoint(
      parents,
      deploys,
      genesis,
      dag,
      defaultStateHash,
      knownStateHashes,
      computeState
    )
  }
}
