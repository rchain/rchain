package coop.rchain.v2.caspernode.state

import cats.syntax.all._
import coop.rchain.v2.casper.data.{CasperData, CasperScope, LatestMessages}
import coop.rchain.v2.casper.syntax.all._
import coop.rchain.v2.casper.{Casper, DependencyGraph, SafetyOracle}
import coop.rchain.v2.caspernode.state.CasperNodeState._
import coop.rchain.v2.caspernode.state.MessageBuffer._

import scala.collection.immutable.Set

/**
 * Full state of the Casper node.
 */
final case class CasperNodeState[M: Ordering, S, U](
    bufferState: MessageBuffer[M],
    validatedState: ValidatedState[M, S, U]
) {

  /**
   * Receive the message.
   * @param message         Message id
   * @param justifications  Message justifications
   * @return                Effects for message receive.
   */
  def ackReceived(message: M, justifications: Set[M]): ReceiveEffect[M, S, U] = {
    val missingSet              = justifications.diff(validatedState.dagSet)
    val ready                   = missingSet.isEmpty
    val (pendingSet, wantedSet) = missingSet.partition(bufferState.contains)
    val newMessageStatus        =
      if (ready) ValidationInProgress
      else AwaitingDependencies(pendingSet, wantedSet)
    val changes                 = Map(message -> newMessageStatus) ++ wantedSet.map(_ -> Requested)
    val newSt                   = copy(bufferState = bufferState ++ changes)
    ReceiveEffect(newSt, changes, wantedSet)
  }

  /**
   * Record result of message validation.
   * @param message      Message id
   * @param casperData   [[CasperData]] for the message.
   * @param blockHeight  Block height, as it is not referenced in Casper, its separate value
   * @param invalid      Result of validation
   * @return             Effects fr message validation
   */
  def ackValidated(
      message: M,
      casperData: CasperData[M, S, U],
      blockHeight: Long,
      invalid: Boolean
  ): ValidateEffect[M, S, U] = {

    /**
     * Update casper buffer.
     */
    // Adjust records in a buffer that depend on the message
    val (newReady, bufferChanges) = bufferState
      .collect {
        case (k, status: AwaitingDependencies[M]) if status.pending.contains(message) =>
          k -> status.copy(pending = status.pending - message)
      }
      .partition { case (_, AwaitingDependencies(pending, wanted)) =>
        pending.isEmpty && wanted.isEmpty
      }
    // Remove message from buffer
    val init                      = bufferState - message
    // Start validating new ready
    val readyRecorded             = newReady.foldLeft(init) { case (acc, (k, _)) =>
      acc + (k -> ValidationInProgress)
    }
    val nextToValidate            = newReady.keySet
    // New buffer state
    val newBufferSt               = bufferChanges.foldLeft(readyRecorded) { case (acc, (k, v)) =>
      acc + (k -> v)
    }

    /**
     * Update validated state.
     */
    // Update messages data map
    val newMessages         = validatedState.messages + (message -> casperData)
    // Update invalid messages
    val newInvalidBlocksSet =
      if (invalid) validatedState.invalidBlocksSet + message else validatedState.invalidBlocksSet
    // Update latest messages
    val newLatestMessages   =
      // do not
      if (invalid) validatedState.latestMessages
      else
        Casper.updateLatestMessages(validatedState.latestMessages, message)(
          newMessages(_).sender,
          newMessages(_).justifications.find(newMessages(_).sender == casperData.sender).get
        )
    // Update height map (might be height map is not necessary)
    val newHeightMap        = validatedState.heightMap.updated(
      blockHeight,
      validatedState.heightMap.getOrElse(blockHeight, Set.empty) + message
    )

    // Update parents map
    val messageParents         = DependencyGraph.computeParents(
      newMessages(message).justifications,
      newMessages(_).justifications
    )
    val newParentsRecord       = message -> messageParents
    val newParentsMap          = validatedState.parentsMap + newParentsRecord
    // Update finalization state Todo think how this can be cached, as only 1 message is added
    val newCasperScopeOpt      =
      if (newLatestMessages == validatedState.latestMessages) validatedState.casperScope.some
      else
        casperImpl(validatedState)
          .findScope(newLatestMessages, -1)
          .right
          .toOption
    assert(newCasperScopeOpt.isDefined, "Cannot find Casper scope after updating latest messages.")
    val newCasperScope         = newCasperScopeOpt.get
    val newlyFinalizedMessages =
      newCasperScope.finalizationFringe diff validatedState.casperScope.finalizationFringe
    // New validated state
    val newValidatedSt         = validatedState.copy(
      messages = newMessages,
      heightMap = newHeightMap,
      latestMessages = newLatestMessages,
      parentsMap = newParentsMap,
      invalidBlocksSet = newInvalidBlocksSet,
      casperScope = newCasperScope
    )
    val newSt                  = copy(validatedState = newValidatedSt, bufferState = newBufferSt)

    ValidateEffect(
      newSt,
      bufferChanges,
      invalid,
      newLatestMessages,
      newCasperScope,
      newlyFinalizedMessages,
      newParentsRecord,
      nextToValidate
    )
  }

  /**
   * Set of messages that has been requested but not received yet.
   */
  def requestedSet: Iterable[M] = bufferState.collect { case (m, Requested) => m }

  /**
   * Whether message is awaiting dependencies.
   */
  def isAwaiting(m: M): Boolean  =
    bufferState.get(m).collect { case AwaitingDependencies(_, _) => true }.nonEmpty

  /**
   * Whether message is now being validated.
   */
  def isPending(m: M): Boolean   =
    bufferState.get(m).collect { case ValidationInProgress => true }.nonEmpty

  /**
   * Whether message is received by the node.
   */
  def isReceived(m: M): Boolean  =
    bufferState.exists { case (t, s) => m == t && s != Requested } || isValidated(m)

  /**
   * Whether message is replayed by the node.
   */
  def isValidated(m: M): Boolean = validatedState.dagSet.contains(m)

  def isEmpty = validatedState.dagSet.isEmpty
}

object CasperNodeState {

  /**
   * Effect of message receive.
   * @param newState      New state of the node.
   * @param bufferChanges Changes to message buffer.
   * @param toRequest     Messages to request from the network.
   */
  final case class ReceiveEffect[M, S, U](
      newState: CasperNodeState[M, S, U],
      bufferChanges: Map[M, MessageStatus],
      toRequest: Set[M]
  )

  /**
   * Effect of message validation.
   * @param newState                New state of the node
   * @param bufferChanges           Changes to message buffer
   * @param invalid                 Whether validation revealed invalid message
   * @param newLatestMessages       New latest messages
   * @param newCasperScope          New Casper scope
   * @param newlyFinalizedMessages  New messages that are finalized w ith advancement of finalization fringe.
   * @param nextToValidate          Messages from buffer for which validated message was the last missing dependency.
   */
  final case class ValidateEffect[M, S, U](
      newState: CasperNodeState[M, S, U],
      bufferChanges: Map[M, MessageStatus],
      invalid: Boolean,
      newLatestMessages: LatestMessages[M, S],
      newCasperScope: CasperScope[M, S],
      newlyFinalizedMessages: Set[M],
      newParentsRecord: (M, Set[M]),
      nextToValidate: Set[M]
  )

  /**
   * Casper instance on [[ValidatedState]].
   */
  def casperImpl[M, S, U](
      validatedState: ValidatedState[M, S, U]
  ): Casper[M, S] = {
    val dg = new DependencyGraph[M, S] {
      override def justifications(message: M): Set[M] = {
        require(
          validatedState.messages.contains(message),
          "No message in database when requesting justifications, Casper cannot proceed."
        )
        validatedState.messages(message).justifications
      }

      override def parents(message: M): Set[M] = {
        require(
          validatedState.messages.contains(message),
          "No message in database when requesting parents, Casper cannot proceed."
        )
        validatedState.parentsMap(message)
      }
      override def sender(message: M): S = {
        require(
          validatedState.messages.contains(message),
          "No message in database when requesting sender, Casper cannot proceed."
        )
        validatedState.messages(message).sender
      }
      override def seqNum(message: M): Long = {
        require(
          validatedState.messages.contains(message),
          "No message in database when requesting seqNum, Casper cannot proceed."
        )
        validatedState.messages(message).seqNum
      }
    }
    val so = new SafetyOracle[M, S] {
      override def compatible(source: M, target: M): Boolean = {
        require(
          validatedState.messages.contains(source),
          "No source message in database when checking compatibility, Casper cannot proceed."
        )
        require(
          validatedState.messages.contains(target),
          "No target message in database when checking compatibility, Casper cannot proceed."
        )
        val sourceSt = validatedState.messages(source).stateMetadata
        val targetSt = validatedState.messages(target).stateMetadata
        sourceSt conflicts targetSt
      }
      override def bondsMap(message: M): Map[S, Long] = {
        require(
          validatedState.messages.contains(message),
          "No target message in database when requesting bonds map, Casper cannot proceed."
        )
        validatedState.messages(message).bondsMap
      }
    }
    Casper(dg, so)
  }
}
