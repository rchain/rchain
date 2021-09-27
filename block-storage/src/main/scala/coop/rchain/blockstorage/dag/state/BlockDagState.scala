package coop.rchain.blockstorage.dag.state

import coop.rchain.blockstorage.dag.state
import coop.rchain.blockstorage.dag.state.BlockDagBufferState._
import coop.rchain.blockstorage.dag.state.BlockDagState._
import coop.rchain.models.BlockHash.BlockHash

import scala.collection.immutable.Set

/** Full state in BlockDagStorage required for processing Casper messages. */
final case class BlockDagState(
    buffer: Buffer,
    validated: BlockDagRepresentationState
) {
  def ackReceived(message: BlockHash, justifications: Set[BlockHash]): AckReceivedResult = {
    val missingSet       = justifications.diff(validated.dagSet)
    val ready            = missingSet.isEmpty
    val toRequest        = missingSet.filterNot(buffer.contains)
    val newMessageStatus = if (ready) ValidationInProgress else AwaitingDependencies(missingSet)
    val changes          = Map(message -> newMessageStatus) ++ toRequest.map(_ -> Requested)
    val newSt            = this.copy(buffer = buffer ++ changes)
    AckReceivedResult(newSt, changes, missingSet, toRequest)
  }

  def ackValidated(
      message: BlockHash,
      newSt: BlockDagRepresentationState
//    offenceOpt: Option[Offence] TODO make this state outputting changes for persistent storage, not vice versa
  ): ValidatedResult = {
    val (newReady, toAdjust) = buffer
      .collect {
        case (k, AwaitingDependencies(missing)) if missing.contains(message) =>
          k -> AwaitingDependencies(missing - message)
      }
      .partition { case (_, AwaitingDependencies(v)) => v.isEmpty }
    val newStatuses = (newReady.toIterator ++ toAdjust).foldLeft(buffer - message) {
      case (acc, (k, AwaitingDependencies(missing))) =>
        if (missing.isEmpty) acc + (k -> ValidationInProgress)
        else acc + (k                 -> AwaitingDependencies(missing))
    }

    // val newValidatedState = offenceOpt.map(_ => validated).getOrElse(validated) // TODO implement state change
    ValidatedResult(state.BlockDagState(newStatuses, newSt), newReady.keySet)
  }

  def wantedSet: Iterable[BlockHash] = buffer.collect { case (m, Requested) => m }

  def received(t: BlockHash): Boolean =
    buffer.exists { case (m, s) => m == t && s != Requested } || validated(t)

  def pendingValidation(t: BlockHash): Boolean =
    buffer.get(t).collect { case AwaitingDependencies(_) => true }.nonEmpty

  def validationInProgress(t: BlockHash): Boolean =
    buffer.get(t).collect { case ValidationInProgress => true }.nonEmpty

  def validated(t: BlockHash): Boolean = validated.dagSet.contains(t)

  def isEmpty = validated.dagSet.isEmpty

}

object BlockDagState {
  final case class AckReceivedResult(
      newState: BlockDagState,
      changes: Map[BlockHash, MessageStatus],
      dependenciesPending: Set[BlockHash],
      dependenciesUnseen: Set[BlockHash]
  )
  final case class ValidatedResult(
      newState: BlockDagState,
      unlockedChildren: Set[BlockHash]
  )
}
