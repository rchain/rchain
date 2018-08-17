package coop.rchain.casper.util.rholang

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import java.io.StringReader

import cats.{Id, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.{trace, Checkpoint}
import monix.execution.Scheduler
import scodec.Codec
import coop.rchain.shared.AttemptOps._
import scodec.bits.BitVector

import scala.collection.immutable
import RuntimeManager.DeployError
import coop.rchain.blockstorage.BlockStore

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(new StringReader(s)).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Monad: BlockStore](b: BlockMessage,
                                                       genesis: BlockMessage,
                                                       dag: BlockDag,
                                                       emptyStateHash: StateHash,
                                                       knownStateHashes: Set[StateHash],
                                                       runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): F[(Option[StateHash], Set[StateHash])] = {
    val tsHash        = ProtoUtil.tuplespace(b)
    val serializedLog = b.body.fold(Seq.empty[Event])(_.commReductions)
    val log           = serializedLog.map(EventConverter.toRspaceEvent).toList
    for {
      blockCheckpointFromDeploys <- computeBlockCheckpointFromDeploys[F](
                                     b,
                                     genesis,
                                     dag,
                                     emptyStateHash,
                                     knownStateHashes,
                                     runtimeManager.replayComputeState(log))
      (computedCheckpoint, _, updatedStateHashes, cost) = blockCheckpointFromDeploys
      computedStateHash                                 = ByteString.copyFrom(computedCheckpoint.root.bytes.toArray)
      result <- if (tsHash.contains(computedStateHash)) {
                 // state hash in block matches computed hash!
                 (Some(computedStateHash) -> updatedStateHashes).pure[F]
               } else {
                 // state hash in block does not match computed hash -- invalid!
                 // return no state hash, do not update the state hash set
                 (None -> knownStateHashes).pure[F]
               }
    } yield result
  }

  def computeDeploysCheckpoint[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      genesis: BlockMessage,
      dag: BlockDag,
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : F[(Checkpoint, Seq[Event], Set[StateHash], Vector[DeployCost])] =
    for {
      //TODO: Revisit how the deployment cost should be handled for multiparent blocks
      //for time being we ignore the `postStateCost`
      parentsPostState <- computeParentsPostState[F](parents,
                                                     genesis,
                                                     dag,
                                                     emptyStateHash,
                                                     knownStateHashes,
                                                     computeState)
      (postStateHash, mergeLog, updatedStateHashes, postStateCost) = parentsPostState
      Right((postDeploysCheckpoint, deployCost))                   = computeState(postStateHash, deploys)
      postDeploysStateHash                                         = ByteString.copyFrom(postDeploysCheckpoint.root.bytes.toArray)
    } yield
      (postDeploysCheckpoint,
       mergeLog.map(EventConverter.toCasperEvent),
       updatedStateHashes + postDeploysStateHash,
       deployCost)

  private def computeParentsPostState[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      genesis: BlockMessage,
      dag: BlockDag,
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : F[(StateHash, Seq[trace.Event], Set[StateHash], Vector[DeployCost])] = {
    val parentTuplespaces: Seq[(BlockMessage, StateHash)] =
      parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of, so use default
      (emptyStateHash, Seq.empty[trace.Event], knownStateHashes, Vector.empty[DeployCost]).pure[F]
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val parentStateHash = parentTuplespaces.head._2
      assert(
        knownStateHashes.contains(parentStateHash),
        "We should have already computed parent state hash when we added the parent to our blockDAG.")
      (parentStateHash, Seq.empty[trace.Event], knownStateHashes, Vector.empty[DeployCost]).pure[F]
    } else {
      for {
        //In the case of multiple parents we need
        //to apply all of the deploys that have been
        //made in all histories since the greatest
        //common ancestor in order to reach the current
        //state.
        gca <- parents.toList.foldM(parents.head) {
                case (gca, parent) =>
                  DagOperations.greatestCommonAncestorF[F](gca, parent, genesis, dag)
              }
        gcaStateHash = ProtoUtil.tuplespace(gca).get

        _ = assert(
          knownStateHashes.contains(gcaStateHash),
          "We should have already computed state hash for GCA when we added the GCA to our blockDAG.")

        // TODO: Fix so that all search branches reach GCA before quitting
        ancestors <- DagOperations
                      .bfTraverseF[F, BlockMessage](parentTuplespaces.map(_._1).toList)(
                        ProtoUtil.unsafeGetParents[F])
                      .toList
        deploys = ancestors
          .takeWhile(_ != gca)
          .flatMap(ProtoUtil.deploys(_).reverse)
          .toIndexedSeq
          .reverse

        //TODO: figure out what casper should do with errors in deploys
        Right((resultStateCheckpoint, deployCost)) = computeState(gcaStateHash, deploys)
        resultStateHash                            = ByteString.copyFrom(resultStateCheckpoint.root.bytes.toArray)
      } yield
        (resultStateHash, resultStateCheckpoint.log, knownStateHashes + resultStateHash, deployCost)
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys[F[_]: Monad: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      emptyStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : F[(Checkpoint, Seq[Event], Set[StateHash], Vector[DeployCost])] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      deploys = ProtoUtil.deploys(b)

      _ = assert(parents.nonEmpty || (parents.isEmpty && b == genesis),
                 "Received a different genesis block.")

      deploysCheckpoint <- computeDeploysCheckpoint[F](
                            parents,
                            deploys,
                            genesis,
                            dag,
                            emptyStateHash,
                            knownStateHashes,
                            computeState
                          )
    } yield deploysCheckpoint
}
