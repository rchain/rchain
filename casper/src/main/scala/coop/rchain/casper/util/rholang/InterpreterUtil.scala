package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.RholangCLI

import java.io.StringReader
import java.nio.file.Path

import monix.execution.Scheduler

import scala.collection.immutable.HashSet

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    RholangCLI.buildNormalizedTerm(new StringReader(s)).runAttempt

  def computeParentsPostState(parents: Seq[BlockMessage],
                              genesis: BlockMessage,
                              dag: BlockDag,
                              tsLocation: Path,
                              tsSize: Long,
                              checkpoints: Map[ByteString, Checkpoint])(
      implicit scheduler: Scheduler): (Tuplespace, Map[ByteString, Checkpoint]) = {
    val parentTuplespaces = parents
      .flatMap(p => {
        for {
          bd <- p.body
          ps <- bd.postState
        } yield (p -> ps.tuplespace)
      })

    if (parentTuplespaces.isEmpty) {
      //no parents to base off of -- start with new tuplespace
      Tuplespace(tsLocation, tsSize) -> checkpoints
    } else if (parentTuplespaces.size == 1) {
      //For a single parent we look up its checkpoint
      val (checkpoint, updatedMap) = checkpoints
        .get(parentTuplespaces.head._2)
        .map(_ -> checkpoints)
        .getOrElse(
          computeBlockCheckpoint(parentTuplespaces.head._1,
                                 genesis,
                                 dag,
                                 tsLocation,
                                 tsSize,
                                 checkpoints))
      checkpoint.toTuplespace -> updatedMap
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

      val (checkpoint, updatedMap) = (for {
        bd <- gca.body
        ps <- bd.postState
        t  = ps.tuplespace
        ch <- checkpoints.get(t)
      } yield (ch -> checkpoints))
        .getOrElse(computeBlockCheckpoint(gca, genesis, dag, tsLocation, tsSize, checkpoints))
      val ts = checkpoint.toTuplespace

      val deploys = DagOperations
        .bfTraverse[BlockMessage](parentTuplespaces.map(_._1))(
          ProtoUtil.parents(_).iterator.map(dag.blockLookup))
        .takeWhile(_ != gca)
        .flatMap(ProtoUtil.deploys(_).reverse)
        .toIndexedSeq
        .reverse
      deploys.foreach(_.term.foreach(ts.addTerm(_)))
      ts -> updatedMap
    }
  }

  def computeDeploysCheckpoint(parents: Seq[BlockMessage],
                               deploys: Seq[Deploy],
                               genesis: BlockMessage,
                               dag: BlockDag,
                               tsLocation: Path,
                               tsSize: Long,
                               checkpoints: Map[ByteString, Checkpoint])(
      implicit scheduler: Scheduler): (Checkpoint, Map[ByteString, Checkpoint]) = {
    val (ts, updatedMap) =
      computeParentsPostState(parents, genesis, dag, tsLocation, tsSize, checkpoints)

    deploys.foreach(d => {
      d.term.foreach(ts.addTerm(_))
    })
    val result = ts.checkpoint
    ts.delete()
    (result, updatedMap + (result.hash -> result))
  }

  def computeBlockCheckpoint(b: BlockMessage,
                             genesis: BlockMessage,
                             dag: BlockDag,
                             tsLocation: Path,
                             tsSize: Long,
                             checkpoints: Map[ByteString, Checkpoint])(
      implicit scheduler: Scheduler): (Checkpoint, Map[ByteString, Checkpoint]) = {

    val preComputedCheckPoint = for {
      bd <- b.body
      ps <- bd.postState
      ts = ps.tuplespace
      ch <- checkpoints.get(ts)
    } yield (ch -> checkpoints)

    preComputedCheckPoint.getOrElse(
      computeBlockCheckpointFromDeploys(b, genesis, dag, tsLocation, tsSize, checkpoints)
    )
  }

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint(b: BlockMessage,
                              genesis: BlockMessage,
                              dag: BlockDag,
                              tsLocation: Path,
                              tsSize: Long,
                              checkpoints: Map[ByteString, Checkpoint])(
      implicit scheduler: Scheduler): (Option[Checkpoint], Map[ByteString, Checkpoint]) = {

    val tsHash = for {
      bd <- b.body
      ps <- bd.postState
    } yield ps.tuplespace

    val (computedCheckpoint, updatedMap) =
      computeBlockCheckpointFromDeploys(b, genesis, dag, tsLocation, tsSize, checkpoints)

    if (tsHash.exists(_ == computedCheckpoint.hash)) {
      //hash in block matches computed hash!
      Some(computedCheckpoint) -> updatedMap
    } else {
      //hash in block does not match computed hash -- invalid!
      //return no checkpoint, do not update the map
      None -> checkpoints
    }
  }

  private def computeBlockCheckpointFromDeploys(b: BlockMessage,
                                                genesis: BlockMessage,
                                                dag: BlockDag,
                                                tsLocation: Path,
                                                tsSize: Long,
                                                checkpoints: Map[ByteString, Checkpoint])(
      implicit scheduler: Scheduler): (Checkpoint, Map[ByteString, Checkpoint]) = {
    val parents = ProtoUtil
      .parents(b)
      .map(dag.blockLookup)

    val deploys = ProtoUtil.deploys(b)

    computeDeploysCheckpoint(
      parents,
      deploys,
      genesis,
      dag,
      tsLocation,
      tsSize,
      checkpoints
    )
  }
}
