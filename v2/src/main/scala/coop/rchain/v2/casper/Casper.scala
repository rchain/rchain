package coop.rchain.v2.casper
import cats.syntax.all._
import coop.rchain.v2.casper.data._
import coop.rchain.v2.casper.syntax.all._

/**
 * @see [[https://github.com/cbc-casper/cbc-casper-paper/blob/master/cbc-casper-paper-draft.pdf Casper draft paper]]
 *      for foundation of this implementation.
 *
 * @param dg            Dependency graph exposing justification structure.
 * @param so            Safety oracle, defining fault tolerances.
 * @param maxComplexity Maximum number of traversals through dependency graph.
 * @tparam M            Type of a message.
 * @tparam S            Type of a sender.
 */
final case class Casper[M, S](
    dg: DependencyGraph[M, S],
    so: SafetyOracle[M, S],
    maxComplexity: Long
) {

  /**
   * @see [[Casper.findScope]]
   */
  def findScope(latestMessages: LatestMessages[M, S], faultToleranceThreshold: Float)(implicit
      ordering: Ordering[M]
  ): Either[Map[S, M], CasperScope[M, S]] =
    Casper.findScope(latestMessages, dg, so, faultToleranceThreshold, maxComplexity)
}

object Casper {

  /**
   * Find [[CasperScope]] - state which cannot be changed by any future messages + set of messages that are unertain yet.
   *
   * If the scope is found - finalized state of the network is identified, along with conflicts scope.
   * Otherwise
   * - network is too diverged (or genesis ceremony not complete) for chosen complexity or
   * - node is still syncing and did not pull enough messages.
   *
   * @param latestMessages          [[LatestMessages]] for all known senders.
   * @param dg                      [[DependencyGraph]] exposing justification structure.
   * @param so                      [[SafetyOracle]] instance.
   * @param faultToleranceThreshold Threshold for declaring finalization.
   * @param maxDepth                Maximum number of traversals through parents. Limits complexity of the Casper.
   * @param ordering                Ordering should be defined for the message.
   * @tparam M                      Type of a message.
   * @tparam S                      Type of a message sender.
   * @return                        Either
   *                                  Incomplete finalization fringe (highest finalized messages for part of senders),
   *                                or
   *                                  Casper scope matching complete finalization fringe.
   */
  def findScope[M, S](
      latestMessages: LatestMessages[M, S],
      dg: DependencyGraph[M, S],
      so: SafetyOracle[M, S],
      faultToleranceThreshold: Float,
      maxDepth: Long = Long.MaxValue
  )(implicit ordering: Ordering[M]): Either[Map[S, M], CasperScope[M, S]] = {
    import dg._

    val allSenders                             = latestMessages.v.keySet
    val latestSeqNums                          = latestMessages.toSet.map(m => sender(m) -> seqNum(m)).toMap
    // This function is used to detect the "highest" message found during traversal.
    // Once N items are pulled from the stream, which is equal to N self justifications, messages
    // higher then seqNum(tip) - N are behind the traversal view, which means no new messages visited can have
    // them as parents anymore.
    // As the first element in message view is a visit of itself, > is used here and not >=.
    def outOfView(message: M, streamIdx: Long) =
      streamIdx > latestSeqNums(sender(message)) - seqNum(message)
    // Accumulator for messages visited while pulling the stream.
    val visitsInit                             = Map.empty[S, Set[M]]
    // Accumulator for finalization fringe: sender -> (message, isHighest)
    val fringeInit                             = Map.empty[S, (M, Boolean)]

    dg.faultTolerances(latestMessages.toSet, so)
      // Stream should be pulled either until the end or until fringe is completed and it is certain that all items
      // are highest.
      .zipWithIndex
      // Accumulate fringe and messages visited.
      .scan((fringeInit, visitsInit)) { case (acc, (iteration, idx)) =>
        iteration.toIterator
          .foldLeft(acc) { case ((fringeAcc, visitsAcc), (m, ft)) =>
            val s                = sender(m)
            // Update visits accumulator
            val newVisitsAcc     =
              visitsAcc.updated(s, visitsAcc.get(s).map(_ + m).getOrElse(Set(m)))
            // Update fringe accumulator
            val isFinalized      = ft > faultToleranceThreshold
            // Current finalized message for sender, should be updated once it is out of scope.
            val curValOpt        = fringeAcc.get(s).map { case (m, _) => m }
            val curValOptUpdated = curValOpt.fold(none[(M, Boolean)]) { cV =>
              val o = outOfView(cV, idx)
              o.guard[Option].as((cV, o))
            }
            // Replace with new value if it is finalized and higher then existing
            val newValOpt        = isFinalized
              .guard[Option]
              .as(m)
              .map { nV =>
                curValOptUpdated
                  .find { case (cV, _) => seqNum(cV) >= seqNum(nV) }
                  .getOrElse((nV, outOfView(nV, idx)))
              }

            val newFringeAcc = newValOpt.map(fringeAcc.updated(s, _)).getOrElse(fringeAcc)
            (newFringeAcc, newVisitsAcc)
          }
      }
      // Fringe is complete when there is highest finalized message for all senders
      .zipWithScan(false) { case (_, (fringeAcc, _)) =>
        val fringeComplete = fringeAcc.collect { case (s, (_, true)) => s }.toSet == allSenders
        !fringeComplete
      }
      // Pull until fringe is complete or stream ends
      .takeThrough { case (_, fringeIsIncomplete) => fringeIsIncomplete }
      // Limit complexity
      .take(maxDepth)
      // Return highest finalized messages found if stream ended or, if complete fringe is found - Casper scope.
      .map { case ((fringe, visited), fringeIsIncomplete) =>
        if (fringeIsIncomplete)
          fringe.collect { case (s, (m, true)) => (s, m) }.asLeft[CasperScope[M, S]]
        else {
          // Clear non finalized set accumulator from messages below the fringe
          val nonFinalizedSet = visited.flatMap { case (sender, messages) =>
            messages.filter(nf => seqNum(nf) > seqNum(fringe(sender)._1))
          }
          CasperScope(
            latestMessages,
            FinalizationFringe(fringe.mapValues { case (m, _) => m }),
            ConflictScope(nonFinalizedSet.toSet)
          ).asRight[Map[S, M]]
        }
      }
      .compile
      .last
      // It's safe to call `get` here as stream `dg.faultTolerances` emits at least one element, so None is not possible.
      .get
  }

  /**
   * Update latest messages given new message observed.
   * @param curV              Current latest messages.
   * @param m                 Message observed.
   * @param sender            Message sender.
   * @param selfJustification Justification of `m` from sender of `m`.
   * @return                  Updated latest messages.
   */
  def updateLatestMessages[M, S](curV: LatestMessages[M, S], m: M)(
      sender: M => S,
      selfJustification: M => M
  ): LatestMessages[M, S] = {
    // Current latest message(s) for a sender
    val curLM = curV.v.find { case (s, _) => s == sender(m) }
    // New value is computed by replace message's self justification (if present) with a message, or just add one more.
    // This logic ensures that if a single message is used as a self justification several times - this results
    // in multiple latest messages for a sender, which is considered as equivocation.
    val mSj   = selfJustification(m)
    val newLM = curLM
      .map { case (s, lms) =>
        val toReplace = lms.find(mSj == _)
        val newLms    = toReplace.map(lms + m - _).getOrElse(lms + m)
        (s, newLms)
      }
      .getOrElse((sender(m), Set(m)))
    LatestMessages(curV.v + newLM)
  }

  /**
   * Sender is equivocating if there are conflicting messages from the same sender found in the CasperScope.
   * This is used to detect equivocation, which is slashable offence.
   */
  def findEquivocatingSenders[M, S](
      casperScope: CasperScope[M, S]
  )(conflictsMap: CasperScope[M, S] => Map[M, Set[M]], sender: M => S): Set[S] =
    // Senders equivocating by sequence (multiple latest messages per sender)
    // TODO sequence equivocating messages can be mergeable and in future this clause might be obsolete
    casperScope.latestMessages.v.collect { case (s, lms) if lms.size != 1 => s }.toSet ++
      // Senders equivocating by the state
      conflictsMap(casperScope)
        .mapValues(_.map(sender))
        .flatMap { case (m, conflictingSenders) =>
          val s = sender(m)
          // Sender is in conflict with itself
          (conflictingSenders contains s).guard[Option].as(s)
        }

  /**
   * Justifications of the message are latest messages as per message view.
   */
  type Justifications[M, S] = LatestMessages[M, S]

  /**
   * Conflict scope cannot be empty, as this means that finalization fringe consists of tip messages.
   */
  case object EmptyConflictScope extends Exception("Conflict scope is empty.")
}
