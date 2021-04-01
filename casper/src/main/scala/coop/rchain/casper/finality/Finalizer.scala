package coop.rchain.casper.finality

import cats.Show
import cats.effect.Concurrent
import cats.implicits._
import coop.rchain.casper.{PrettyPrinter, SafetyOracle}
import coop.rchain.dag.DagReader
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

object Finalizer {

  /**
    * Run finalization through children until no new finalized child found.
    * @param dag - dependency DAG
    * @param startHash - starting point for finalization, should be some hash directly finalized before
    * @param finalisationEffect - effect to be executed when some hash is finalized (pass fault tolerance threshold)
    * @param lastFinalisedAdvanceEffect - effect to be executed when `last finalised` advances.
    *                                  NOTE: even if some children exceed finalization threshold, `last finalised`
    *                                  might not advance if there are some additional constraints put on
    *                                  `last finalised` compared to just `finalized`
    * @param computeFaultTolerance - function to compute fault tolerance
    * @param faultToleranceThreshold - fault tolerance threshold for finalization
    * @return Optional new `last finalized`
    */
  def run[F[_]: Concurrent: Log, A](dag: DagReader[F, A])(
      startHash: A,
      isFinalisedF: A => F[Boolean],
      finalisationEffect: A => F[Unit],
      lastFinalisedAdvanceEffect: A => F[Unit],
      computeFaultTolerance: A => F[Float],
      faultToleranceThreshold: Float
  )(implicit orderingA: Ordering[A], show: Show[A]): F[Option[A]] = {

    // ordering for sorting finalized children by fault tolerance
    implicit val o: Ordering[(Float, A)] = Ordering.Tuple2(
      Ordering[Float].reverse,
      orderingA
    )

    def computeNextLFBWithEffects(currLFB: A): F[Option[A]] =
      Stream
      // get all children for current LFB
        .eval(dag.children(currLFB).map(_.getOrElse(Set.empty)))
        // flatten in a single stream
        .flatMap(xs => Stream.fromIterator(xs.iterator))
        // compute fault tolerances for children
        .parEvalMapProcBounded { child =>
          computeFaultTolerance(child).map(ft => (child, ft))
        }
        // find children that have to be finalized
        .filter { case (_, faultTolerance) => faultTolerance > faultToleranceThreshold }
        // find all parents of newly finalised children that are not finalized yet
        .parEvalMapProcBounded {
          case v @ (child, _) =>
            Log[F].debug(s"Finalizing child ${child.show}") *>
              dag
                .gatherAncestors(child, v => isFinalisedF(v).not)
                // indirectly finalized block fault tolerance is not of interest, so setting it to not finalized value
                .map(ifc => ifc.toList.map(i => (i, SafetyOracle.MIN_FAULT_TOLERANCE)) :+ v)
        }
        // reorg in a single stream
        .flatMap(xs => Stream.fromIterator(xs.iterator))
        // run effects for all finalized hashes
        .parEvalMapProcBounded { case v @ (h, _) => finalisationEffect(h).as(v) }
        .compile
        .toList
        // sort newly finalised hashes by fault tolerance, return head option
        // output new LFB
        .map(_.sortBy { case (hash, f) => (f, hash) }.headOption.map(_._1))

    // make sure starting point exists in DAG
    val checkArgs = dag.children(startHash).map(_.isEmpty) >>= { hasNoChildren =>
      Concurrent[F]
        .raiseError(
          new IllegalArgumentException(
            "Starting point for finalization is not found in DAG or does not have children"
          )
        )
        .whenA(hasNoChildren)
    }

    val finalizationUnfold =
      Stream
        .unfoldLoopEval(startHash)(computeNextLFBWithEffects(_).map(x => (x, x)))
        .unNoneTerminate
        .evalTap(lastFinalisedAdvanceEffect)

    checkArgs *> finalizationUnfold.compile.last
  }
}
