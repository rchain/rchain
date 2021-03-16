package coop.rchain.casper.finality

import cats.effect.Concurrent
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.SafetyOracle
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue

object Finalizer {

  // ordering for sorting finalized children by fault tolerance
  implicit val o = Ordering.Tuple2(
    Ordering[Float].reverse,
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)
  )

  /**
    * Run finalization through children until no new finalized child found
    * @param dag
    * @param startHash - starting point for finalization, should be some hash directly finalized before
    * @param finaliseEffect - effect to be executed when some hash is finalized
    * @return None if LFB stays the same, new hash otherwise
    */
  // format: off
  def run[F[_]
  /* Execution */ : Concurrent
  /* Casper */    : SafetyOracle
  /* Diag */      : Log] // format: on
  (
      dag: BlockDagRepresentation[F],
      startHash: BlockHash,
      finaliseEffect: BlockHash => F[Unit],
      computeFaultTolerance: (BlockDagRepresentation[F], BlockHash) => F[Float],
      faultToleranceThreshold: Float
  ): F[Option[BlockHash]] = {

    def computeNextLFBWithEffects(currLFB: BlockHash): F[BlockHash] =
      Stream
      // get all children for current LFB
        .eval(dag.children(currLFB).map(_.getOrElse(Set.empty)))
        // flatten in a single stream
        .flatMap(xs => Stream.fromIterator(xs.iterator))
        // compute fault tolerances for children
        .parEvalMapProcBounded { child =>
          computeFaultTolerance(dag, child).map(ft => (child, ft))
        }
        // find children that have to be finalized
        .filter { case (_, faultTolerance) => faultTolerance >= faultToleranceThreshold }
        // find all parents of newly finalised children that are not finalized yet
        .parEvalMapProcBounded {
          case v @ (child, _) =>
            dag
              .getNonFinalizedAncestors(child)
              // indirectly finalized block fault tolerance is not of interest, so setting it to not finalized value
              .map(ifc => ifc.toList.map(i => (i, SafetyOracle.MIN_FT)) :+ v)
        }
        // reorg in a single stream
        .flatMap(xs => Stream.fromIterator(xs.iterator))
        // run effects for all finalized hashes
        .parEvalMapProcBounded { case v @ (h, _) => finaliseEffect(h).as(v) }
        .compile
        .toList
        // sort newly finalised hashes by fault tolerance, return head
        .map(_.maxBy { case (hash, ft) => (ft, hash) })
        // output new LFB
        .map { case (hash, _) => hash }

    for {
      // make sure starting point exists in DAG
      _ <- dag.lookup(startHash).map(h => assert(h.isDefined))
      // queue of current LFB's
      finQueue <- Queue.bounded[F, BlockHash](maxSize = 1)
      // put start LFB in queue
      _ <- finQueue.enqueue1(startHash)
      r <- finQueue.dequeue
          // run next LFB calculation,
            .evalMap(lfb => computeNextLFBWithEffects(lfb))
            // passing result back to input queue.
            .evalTap(finQueue.enqueue1)
            .compile
            // stream will end once no new LFB found, the last LFB is the result
            // effects for all finalised hashes are executed
            .last
    } yield r
  }
}
