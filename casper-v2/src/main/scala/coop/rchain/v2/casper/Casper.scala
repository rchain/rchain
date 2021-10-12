package coop.rchain.v2.casper
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.v2.casper.data._
import coop.rchain.v2.casper.syntax.all._

final case class Casper[F[_], M, S](
    dg: DependencyGraph[F, M, S],
    so: SafetyOracle[F, M, S],
    maxComplexity: Long
) {

  def computeScope(
      latestMessages: LatestMessages[M, S],
      faultToleranceThreshold: Float
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[CasperScope[M, S]] =
    Casper.computeScope(latestMessages, dg, so, faultToleranceThreshold, maxComplexity)
}

object Casper {

  /**
   * Compute Casper scope.
   */
  def computeScope[F[_], M, S](
      latestMessages: LatestMessages[M, S],
      dg: DependencyGraph[F, M, S],
      so: SafetyOracle[F, M, S],
      faultToleranceThreshold: Float,
      maxDepth: Long
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[CasperScope[M, S]] = {
    import dg._

    val allSenders    = latestMessages.v.keySet
    val latestSeqNums = latestMessages.toSet.map(m => sender(m) -> seqNum(m)).toMap
    // Accumulator for messages visited while pulling the stream.
    val visitsInit    = Map.empty[S, Set[M]]
    // Accumulator for finalization fringe
    val fringeInit    = Map.empty[S, M]

    dg.faultTolerances(latestMessages.toSet, so)
      // Accumulate fringe and messages visited
      .scan((fringeInit, visitsInit)) { case ((fringeAcc, visitsAcc), level) =>
        val finalized    = level.collect { case (m, ft) if ft > faultToleranceThreshold => m }
        val newFringeAcc = finalized.foldLeft(fringeAcc) { case (acc, m) =>
          val s                         = sender(m)
          val higherMessageAlreadyFound = acc.get(s).exists(seqNum(_) >= seqNum(m))
          if (higherMessageAlreadyFound) acc else acc.updated(s, m)
        }
        val newVisitsAcc = level.foldLeft(visitsAcc) { case (acc, (m, _)) =>
          val s = sender(m)
          acc.updated(s, acc.get(s).map(_ + m).getOrElse(Set(m)))
        }
        (newFringeAcc, newVisitsAcc)
      }
      // Pull stream until fringe is completed and it is certain that all items are highest.
      // Once N items are pulled from stream, which is N justifications, no one can reference messages higher then
      // seqNum(tip) - N. Therefore no results higher then already found.
      .zipWithIndex
      // Limit Casper complexity
      .take(maxDepth)
      .find { case ((fringeAcc, _), idx) =>
        val fringeComplete  = fringeAcc.keySet == allSenders
        val fringeIsHighest =
          // As first element in message view is a visit of itself, here `>` is used and not `>=`.
          fringeAcc.valuesIterator.forall(m => idx > latestSeqNums(sender(m)) - seqNum(m))
        fringeComplete && fringeIsHighest
      }
      // Clear non finalized set accumulator from messages below the fringe
      .map { case ((fringe, visited), _) =>
        val nonFinalizedSet = visited.flatMap { case (sender, messages) =>
          messages.filter(nf => seqNum(nf) > seqNum(fringe(sender)))
        }
        CasperScope(
          latestMessages,
          FinalizationFringe(fringe.values.toSet).some,
          ConflictScope(nonFinalizedSet.toSet)
        )
      }
      .evalTap(_.conflictScope.v.nonEmpty.guard[Option].liftTo(EmptyConflictScope))
      .compile
      .last
      // Throw exception if no fringe found
      .flatMap(_.liftTo(NoFinalizationFringe))
  }

  /**
   * Logic for updating latest messages when new message arrives.
   */
  def updateLatestMessages[M, S](latestMessages: LatestMessages[M, S], newMessage: M)(
      seqNum: M => Long,
      sender: M => S
  ): Option[LatestMessages[M, S]] = ???

  /**
   * Senders that expose sequence equivocation.
   */
  def sequenceEquivocatingSenders[M, S](lms: LatestMessages[M, S]): Iterable[S] =
    lms.v.filter { case (_, ms) => ms.size > 1 }.keys

  /**
   * Detect whether message is lazy.
   */
  def lazyMessage[M, S](
      finalizationFringe: FinalizationFringe[M],
      justifications: Justifications[M, S]
  )(
      seqNum: M => Long,
      sender: M => S
  ): Boolean                                                                    = ???

  def computeJustifications[M, S](latestMessages: LatestMessages[M, S]) = ???

  // Casper cannot find finalization fringe. Complexity should be relaxed.
  case object NoFinalizationFringe extends Exception("Unable to find Finalization Fringe.")

  // Conflict scope cannot be empty, as this means that finalization fringe consists of tip messages.
  case object EmptyConflictScope extends Exception("Conflict scope is empty.")
}
