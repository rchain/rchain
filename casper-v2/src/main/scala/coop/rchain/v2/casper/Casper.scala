package coop.rchain.v2.casper
import cats.syntax.all._
import coop.rchain.v2.casper.data._
import coop.rchain.v2.casper.syntax.all._

final case class Casper[M, S](
    dg: DependencyGraph[M, S],
    so: SafetyOracle[M, S],
    maxComplexity: Long
) {
  def findScope(latestMessages: LatestMessages[M, S], faultToleranceThreshold: Float)(implicit
      ordering: Ordering[M]
  ): Either[FinalizationFringe[M], CasperScope[M, S]] =
    Casper.findScope(latestMessages, dg, so, faultToleranceThreshold, maxComplexity)
}

object Casper {

  /**
   * Find [[CasperScope]].
   *
   * If the scope is found - finalized state of the network is identified.
   * Otherwise
   * - network is too diverged (or genesis ceremony not complete) for chosen complexity or
   * - node is still syncing and did not pull enough messages.
   *
   * @param latestMessages          [[LatestMessages]] for all known senders.
   * @param dg                      [[DependencyGraph]] exposing justification structure.
   * @param so                      [[SafetyOracle]] instance.
   * @param faultToleranceThreshold Threshold for declaring finalization.
   * @param maxDepth                Maximum number of traversals through parents.
   *                                Limits complexity of the Casper. Default is unlimited.
   * @param ordering                Ordering should be defined for the message.
   * @tparam M                      Type of a message.
   * @tparam S                      Type of a message sender.
   * @return                        Either incomplete finalization fringe, or (of fringe is complete) - Casper scope.
   *                                Fringe is complete when there is highest finalized message for all known senders,
   *                                so finalized state of the network can be identified.
   */
  def findScope[M, S](
      latestMessages: LatestMessages[M, S],
      dg: DependencyGraph[M, S],
      so: SafetyOracle[M, S],
      faultToleranceThreshold: Float,
      maxDepth: Long = Long.MaxValue
  )(implicit ordering: Ordering[M]): Either[FinalizationFringe[M], CasperScope[M, S]] = {
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
            val newVisitsAcc     = visitsAcc.updated(s, visitsAcc.get(s).map(_ + m).getOrElse(Set(m)))
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
      // Return incomplete finalization fringe if stream ended or, if complete fringe is found - complete Casper scope.
      .map { case ((fringe, visited), fringeIsIncomplete) =>
        val fringeMessages = fringe.mapValues { case (m, _) => m }.values.toSet
        if (fringeIsIncomplete) FinalizationFringe(fringeMessages).asLeft[CasperScope[M, S]]
        else {
          // Clear non finalized set accumulator from messages below the fringe
          val nonFinalizedSet = visited.flatMap { case (sender, messages) =>
            messages.filter(nf => seqNum(nf) > seqNum(fringe(sender)._1))
          }
          CasperScope(
            latestMessages,
            FinalizationFringe(fringeMessages),
            ConflictScope(nonFinalizedSet.toSet)
          ).asRight[FinalizationFringe[M]]
        }
      }
      .compile
      .last
      // It's safe to call `get` here as stream `dg.faultTolerances` emits at least one element, so None is not possible.
      .get
  }

  /**
   * Message M is lazy if it uses justifications below finalization fringe
   * AND no messages of finalization fringe sees M (use M OR descendants as justification).
   */
  def lazyMessage[M, S](
      finalizationFringe: FinalizationFringe[M],
      justifications: Justifications[M, S]
  )(
      seqNum: M => Long,
      sender: M => S
  ): Boolean = justifications.v.exists { case (sender, message) => finalizationFringe.v.map() }

  /**
   * Senders that are active as per finalized state of the network. This is used to prevent DoS attacks via spamming
   * the messages having correct signatures, but from random senders.
   *
   * Senders that are expected to be added on top of the DAG (we are not interested in others once finalization fringe
   * is found) should come from senders that are active as per finalized state of the network.
   * From the standpoint of bonding this means that once a new sender is bonded and becomes active after epoch change,
   * it have to wait for epoch change to be finalized and recorded in bonds maps of all messages of the finalization
   * fringe.
   */
  def activeSenders[M, S](finalizationFringe: FinalizationFringe[M])(
      activeSenders: M => Set[S]
  ): Set[S] =
    finalizationFringe.v.flatMap(activeSenders)

  def computeJustifications[M, S](latestMessages: LatestMessages[M, S]) = ???

  // Casper cannot find finalization fringe. Complexity should be relaxed.
  case object NoFinalizationFringe extends Exception("Unable to find Finalization Fringe.")

  // Conflict scope cannot be empty, as this means that finalization fringe consists of tip messages.
  case object EmptyConflictScope extends Exception("Conflict scope is empty.")
}
