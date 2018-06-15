package coop.rchain.casper.util.rholang

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import monix.execution.Scheduler

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  def computeDeploysCheckpoint(parents: Seq[BlockMessage],
                               deploys: Seq[Deploy],
                               genesis: BlockMessage,
                               dag: BlockDag,
                               defaultStateHash: StateHash,
                               knownStateHashes: Set[StateHash],
                               runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {
    val (postStateHash, updatedStateHashes) =
      computeParentsPostState(parents,
                              genesis,
                              dag,
                              defaultStateHash,
                              knownStateHashes,
                              runtimeManager)

    val Right(postDeploysStateHash) =
      runtimeManager.computeState(postStateHash, deploys.flatMap(_.term).toList)
    (postDeploysStateHash, updatedStateHashes + postDeploysStateHash)
  }

  def computeBlockCheckpoint(b: BlockMessage,
                             genesis: BlockMessage,
                             dag: BlockDag,
                             defaultStateHash: StateHash,
                             knownStateHashes: Set[StateHash],
                             runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {

    val blockStateHash = ProtoUtil.tuplespace(b).get
    if (knownStateHashes.contains(blockStateHash)) {
      (blockStateHash, knownStateHashes)
    } else {
      computeBlockCheckpointFromDeploys(b,
                                        genesis,
                                        dag,
                                        defaultStateHash,
                                        knownStateHashes,
                                        runtimeManager)
    }
  }

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint(b: BlockMessage,
                              genesis: BlockMessage,
                              dag: BlockDag,
                              defaultStateHash: StateHash,
                              knownStateHashes: Set[StateHash],
                              runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (Option[StateHash], Set[StateHash]) = {

    val tsHash = ProtoUtil.tuplespace(b)

    val (computedStateHash, updatedStateHashes) =
      computeBlockCheckpointFromDeploys(b,
                                        genesis,
                                        dag,
                                        defaultStateHash,
                                        knownStateHashes,
                                        runtimeManager)

    if (tsHash.contains(computedStateHash)) {
      // state hash in block matches computed hash!
      Some(computedStateHash) -> updatedStateHashes
    } else {
      // state hash in block does not match computed hash -- invalid!
      // return no state hash, do not update the state hash set
      None -> knownStateHashes
    }
  }

  private def computeParentsPostState(parents: Seq[BlockMessage],
                                      genesis: BlockMessage,
                                      dag: BlockDag,
                                      defaultStateHash: StateHash,
                                      knownStateHashes: Set[StateHash],
                                      runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (defaultStateHash, knownStateHashes)
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      if (knownStateHashes.contains(parentStateHash)) {
        (parentStateHash, knownStateHashes)
      } else {
        computeBlockCheckpoint(parentTuplespaces.head._1,
                               genesis,
                               dag,
                               defaultStateHash,
                               knownStateHashes,
                               runtimeManager)
      }
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
      val (computedGcaStateHash, _) =
        if (knownStateHashes.contains(gcaStateHash)) {
          (gcaStateHash, knownStateHashes)
        } else {
          computeBlockCheckpoint(gca,
                                 genesis,
                                 dag,
                                 defaultStateHash,
                                 knownStateHashes,
                                 runtimeManager)
        }

      val deploys = DagOperations
        .bfTraverse[BlockMessage](parentTuplespaces.map(_._1))(
          ProtoUtil.parents(_).iterator.map(dag.blockLookup))
        .takeWhile(_ != gca)
        .flatMap(ProtoUtil.deploys(_).reverse)
        .toIndexedSeq
        .reverse

      //TODO: figure out what casper should do with errors in deploys
      val Right(resultStateHash) =
        runtimeManager.computeState(computedGcaStateHash, deploys.flatMap(_.term).toList)
      (resultStateHash, knownStateHashes + resultStateHash)
    }
  }

  private def computeBlockCheckpointFromDeploys(b: BlockMessage,
                                                genesis: BlockMessage,
                                                dag: BlockDag,
                                                defaultStateHash: StateHash,
                                                knownStateHashes: Set[StateHash],
                                                runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): (StateHash, Set[StateHash]) = {
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
      runtimeManager
    )
  }
}
