package coop.rchain.casper.v2.core.syntax

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.v2.core.Casper._
import coop.rchain.casper.v2.core._
import coop.rchain.casper.v2.core.syntax.all._

trait CasperSyntax {
  implicit final def casperSyntax[F[_], M, S](c: Casper[F, M, S]): CasperOps[F, M, S] =
    new CasperOps[F, M, S](c)
}

final class CasperOps[F[_], M, S](val c: Casper[F, M, S]) extends AnyVal {
  import c._

  def messageScope(
      latestMessages: Set[M],
      dag: DependencyGraph[F, M, S],
      safetyOracle: SafetyOracle[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[MessageScope[M]] = {
    import dag._

    val allSenders    = latestMessages.map(sender)
    val latestSeqNums = latestMessages.map(m => sender(m) -> seqNum(m)).toMap
    val visitsInit    = Map.empty[S, Set[M]] // Accumulator for messages visited while pulling the stream.
    val fringeInit    = Map.empty[S, M] // Accumulator for finalization fringe

    safetyOracle
      .faultTolerances(latestMessages, dag)
      // Accumulate fringe and messages visited
      .scan((fringeInit, visitsInit)) {
        case ((fringeAcc, visitsAcc), level) =>
          val finalized = level.collect { case (m, ft) if ft > faultToleranceThreshold => m }
          val newFringeAcc = finalized.foldLeft(fringeAcc) {
            case (acc, m) =>
              val s                      = sender(m)
              val shouldRecordFringeItem = !acc.get(s).exists(seqNum(_) >= seqNum(m))
              if (shouldRecordFringeItem) acc.updated(s, m) else acc
          }
          val newVisitsAcc = level.foldLeft(visitsAcc) {
            case (acc, (m, _)) =>
              val s = sender(m)
              acc.updated(s, acc.get(s).map(_ + m).getOrElse(Set(m)))
          }
          (newFringeAcc, newVisitsAcc)
      }
      // Pull stream until fringe is completed and it is certain that all items are highest.
      // Once N items are pulled from stream, which is N justifications, no one can reference messages higher then
      // seqNum(tip) - N. Therefore no results higher then already found.
      .zipWithIndex
      .find {
        case ((fringeAcc, _), idx) =>
          val fringeComplete = fringeAcc.keySet == allSenders
          val fringeIsHighest =
            fringeAcc.valuesIterator.forall(m => idx >= latestSeqNums(sender(m)) - seqNum(m))
          fringeComplete && fringeIsHighest
      }
      // Clear non finalized set accumulator from messages below the fringe
      .map {
        case ((fringe, visited), _) =>
          val nonFinalizedSet = visited.flatMap {
            case (sender, messages) => messages.filter(nf => seqNum(nf) > seqNum(fringe(sender)))
          }
          MessageScope(
            FinalizationFringe(fringe.values.toSet),
            ConflictScope(nonFinalizedSet.toSet)
          )
      }
      .compile
      .last
      // Throw exception if no fringe found
      .flatMap(_.liftTo(NoFinalizationFringe))
  }
}
