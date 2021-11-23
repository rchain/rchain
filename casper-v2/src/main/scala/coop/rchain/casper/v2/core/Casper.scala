package coop.rchain.casper.v2.core
import coop.rchain.casper.v2.core.DependencyGraph.LatestMessages

trait Casper[F[_], M, S] {

  /**
    * Threshold for finalization.
    */
  def faultToleranceThreshold: Float

  /** Constraints Casper scope to limit complexity. */
  val maxDepth: Long
}

object Casper {

  /** Highest finalized messages from all known senders. Defines finalized state of the network. */
  type FinalizationFringe[S, M] = LatestMessages[S, M]

  /** All messages above the finalization fringe. */
  final case class ConflictScope[M](v: Set[M])

  /**
    * Scope of the message.
    * @param finalizationFringe  [[FinalizationFringe]] that represents finalized state of the network.
    *                            Fringe can be merged or used as a key to get access to actual state.
    * @param conflictScope       [[ConflictScope]]
    * @tparam M                  Type of the message.
    */
  final case class MessageScope[S, M](
      finalizationFringe: FinalizationFringe[S, M],
      conflictScope: ConflictScope[M]
  )

  /**
    * Can be thrown in two cases:
    * 1. Genesis ceremony is in progress.
    *   Validators are producing blocks on top of the genesis, but there are not enough blocks exchanged in the network,
    *   so no finalization fringe can be found. In this case max complexity of safety oracle puts a constraint on
    *   length of genesis ceremony. If genesis is out of the view of latest messages and no finalization fringe found,
    *   this is a reason to declare genesis ceremony failed.
    * 2. Silent validator. Safety oracle complexity puts a constraint on how long Casper can tolerate validator not
    *   producing blocks. If validator is silent for long enough - at some point his latest message becomes out of the
    *   view of other latest messages, therefore cannot be finalized. This means finalization fringe cannot be completed.
    *   This might cause a network halt, or some reaction to such silent validator.
    *
    * For now, as in general traversing through justifications should not be heavy task, and can be cached,
    * it is suggested to just relax maxDepth, up tp Long.MaxValue.
    */
  val noFringeMsg = "Unable to find Finalization Fringe."
  case object NoFinalizationFringe extends Exception(noFringeMsg)

  /**
    * Thrown when conflict scope of the message is empty.
    */
  val emptyConflictScopeMsg = "Conflict scope is empty."
  case object EmptyConflictScope extends Exception(emptyConflictScopeMsg)
}
