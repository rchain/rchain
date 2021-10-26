package coop.rchain.v2.caspernode

import cats.syntax.all.none
import coop.rchain.v2.casper.Casper
import coop.rchain.v2.casper.Casper.updateLatestMessages
import coop.rchain.v2.casper.data.{CasperScope, FinalizationFringe, LatestMessages}
import coop.rchain.v2.casper.stcasper.StateMessage
import coop.rchain.v2.caspernode.CasperNodeState._

/**
 * State of the Casper node.
 *
 * Casper node follows the consensus and (optionally) maintains the full global state of the shard.
 *
 * @param latestMessages        [[LatestMessages]] observed from all known senders.
 * @param justificationsMap     Justifications for a message.
 * @param parentsMap            Parents for a message.
 * @param childrenMap           Children for a message.
 * @param finalizationFringe    Finalization fringe.
 * @param casperScopeOpt        [[CasperScope]] defining the state of the network. If None, scope cannot be found yet.
 * @param validationFringe      Latest validated messages.
 * @param slashings             Slashings, produced by [[Validator]].
 * @param metadataMap           Metadata for a message, required for Casper.
 * @param dependenciesRequested Set of messages requested as dependencies but not not received yet
 * @param processingSet         Set of messages that are currently in process of validation.
 * @param validatedSet          Set of messages that are validated.
 * @tparam M                    Type for message id.
 * @tparam U                    Type for minimal state unit.
 * @tparam S                    Type for message sender.
 */
case class CasperNodeState[M: Ordering, U, S](
    // Network observed
    latestMessages: LatestMessages[M, S],
    justificationsMap: Map[M, List[M]],
    parentsMap: Map[M, List[M]],
    finalizationFringe: FinalizationFringe[M, S],
    casperScopeOpt: Option[CasperScope[M, S]],
    // Network validated
    validationFringe: ValidationFringe[M, S],
    activeSenders: Set[S],
    slashings: Set[Slashing[M]],
    // Metadata store
    metadataMap: Map[M, CasperMessageData[M, U, S]],
    // State of processing
    dependenciesRequested: Set[M],
    validatingSet: Set[M],
    validatedSet: Set[M]
) { self =>

  /**
   * Acknowledge message observation.
   * Messages piped into this call should have valida signature
   *
   * @param m         Message observed.
   * @param metadata  Message metadata.
   * @return          New state and effects that message observation can produce.
   */
  def ackObserved(
      m: M,
      metadata: CasperMessageData[M, U, S]
  ): (CasperNodeState[M, U, S], ObservationEffects[M, S]) = {
    // Update metadata map
    val newMetadataMap                                   = metadataMap.updated(m, metadata)
    // Update latest messages
    val newLatestMessagesOpt                             =
      updateLatestMessages(latestMessages, m)(
        newMetadataMap(_).seqNum,
        newMetadataMap(_).sender
      )
    // Remove message from requested set, add missing set
    val missingSet                                       =
      metadata.justifications diff validatedSet diff dependenciesRequested diff validatingSet
    val newDependenciesRequested                         = dependenciesRequested - m ++ missingSet
    // Update finalization if
    // 1. Latest messages are changed, or
    // 2. Finalization fringe is incomplete and observed message might become part of it.
    val updateFinalization                               = newLatestMessagesOpt.isDefined ||
      !(finalizationFringe.v.map(metadataMap(_).sender) contains metadata.sender)
    val (newFinalizationFringeOpt, newOptCasperScopeOpt) =
      if (updateFinalization)
        casperImpl(self)(metadataMap(_).stateMetadata)
          .findScope(newLatestMessagesOpt.getOrElse(latestMessages), -1)
          .map(cs => (cs.finalizationFringe.some, cs.some.some))
          .leftMap(ff => (ff.some, none[Option[CasperScope[M, S]]]))
          .merge
      else
        (none[FinalizationFringe[M]], none[Option[CasperScope[M, S]]])
    val readyForValidation                               = (metadata.justifications diff validatedSet).isEmpty
    (
      copy(
        dependenciesRequested = newDependenciesRequested,
        metadataMap = newMetadataMap,
        latestMessages = newLatestMessagesOpt.getOrElse(self.latestMessages),
        finalizationFringe = newFinalizationFringeOpt.getOrElse(self.finalizationFringe),
        casperScopeOpt = newOptCasperScopeOpt.getOrElse(self.casperScopeOpt)
      ),
      ObservationEffects(
        m,
        newLatestMessagesOpt,
        newFinalizationFringeOpt,
        newOptCasperScopeOpt,
        readyForValidation,
        missingSet
      )
    )
  }

  /**
   * Acknowledge validation of the message.
   *
   * @param m           The message.
   * @param offenceOpt  (optional) Offence found.
   * @return            New state and effects that validation can trigger.
   */
  def ackValidated(
      m: M,
      offenceOpt: Option[Offence]
  ): (CasperNodeState[M, U, S], ValidationEffects[M]) = {
    val slashingOpt         = offenceOpt.collect { case so: SlashableOffence =>
      Slashing(m, so)
    }
    val newSlashings        = slashingOpt.map(slashings + _).getOrElse(slashings)
    val newValidationFringe =
      updateLatestMessages(validationFringe, m)(metadataMap(_).seqNum, metadataMap(_).sender)
        .getOrElse(validationFringe)
    val newValidatedSet     = validatedSet + m
    val unlockedChildren    = parentsMap
      .filterKeys(m => !validatedSet.contains(m))
      .filter { case (_, parents) =>
        (parents.toSet diff validatedSet).isEmpty
      }
      .keys
      .toList
    (
      copy(
        slashings = newSlashings,
        validationFringe = newValidationFringe,
        validatedSet = newValidatedSet
      ),
      ValidationEffects(m, slashingOpt, unlockedChildren)
    )
  }

  def ignoreMessage(m: M)(sender: M => S): Boolean = unexpectedSenders(m)(activeSenders, sender)

  /**
   * Only messages from expected senders should be observed. Others are ignored.
   * This is the second layer or DoS attack protection, after the signature verification.
   */
  def unexpectedSenders(m: M)(activeSenders: M => Set[S], sender: M => S): Boolean =
    !Casper.activeSenders(finalizationFringe)(activeSenders).contains(sender(m))

  /**
   * Node is in sync mode iif
   * 1. CasperScope is None, which means there are not enough messages yet in the local view to find the
   *    latest Casper scope.
   * 2. There are dependencies requested but not received, which means that there are gaps in dependency graph, so
   *    even if Casper scope is defined, it might be outdated. Note: Casper scope can be either defined or not.
   */
  def syncMode: Boolean = casperScopeOpt.isEmpty || dependenciesRequested.nonEmpty

  /**
   * Message creation is prohibited when node is in sync mode, because there is a risk to create lazy message that
   * is in conflict with finalized state.
   */
  def proposeAllowed: Boolean = !syncMode

  /**
   * Node should pause validation when in sync mode, as network view is incomplete.
   */
  def validationAllowed: Boolean = !syncMode

}

object CasperNodeState {

  /**
   * Casper instance on [[CasperNodeState]].
   */
  def casperImpl[M, U, S](
      casperNodeState: CasperNodeState[M, U, S],
      complexity: Int = 100
  )(stM: M => StateMessage[U]): Casper[M, S] = {
    val dg = new DependencyGraph[M, S] {
      override def justifications(message: M): Set[M] = casperNodeState.justificationsMap _

      override def parents(message: M): Set[M] = casperNodeState.parentsMap _

      override def sender(message: M): S = casperNodeState.metadataMap(message).sender

      override def seqNum(message: M): Long = casperNodeState.metadataMap(message).seqNum
    }
    val so = new SafetyOracle[M, S] {
      override def compatible(source: M, target: M): Boolean = stM(source).conflicts(stM(target))

      override def bondsMap(message: M): Map[S, Long] = bondsMap _
    }
    Casper(dg, so, complexity)
  }

  trait FinalizationTransition

  /**
   * Finalization fringe is changed. No Casper scope is found yet.
   */
  final case class FinalizationFringeChanged[M](newV: FinalizationFringe[M])

  /**
   * Casper scope is advanced. A new message added on top of observable DAG might advance finalization fringe,
   * so casper scope.
   */
  final case class CasperScopeAdvanced[M, S](casperScope: CasperScope[M, S])
      extends FinalizationTransition

  /**
   * Casper scope is destroyed. This happens when node is cut off for some time from the network
   * and found latest messages disconnected from tips of observable DAG.
   */
  final case object CasperScopeDestroyed extends FinalizationTransition

  /**
   * Effects that observing a message might produce:
   * 1. Add message to the state.
   * 2. Update latest messages.
   * 3. Update finalization fringe.
   * 4. Casper scope. New message received can destroy the scope when it turn out that current view is outdated.
   * That's why its Option[Option], Some(None) means casper scope is destroyed.
   * When 1/3+ of the stake send latest messages disconnected from tips of connected DAG,
   * there is not enough stake to maintain current scope.
   * 5. Forward message to validation if all dependencies are already validated.
   * 6. Request missing dependencies if there are some.
   */
  case class ObservationEffects[M, S](
      message: M,
      newLatestMessages: Option[LatestMessages[M, S]],
      newFinalizationFringe: Option[FinalizationFringe[M, S]],
      newCasperScopeOpt: Option[Option[CasperScope[M, S]]],
      forwardToValidator: Boolean,
      dependenciesMissing: Set[M]
  )

  /**
   * Effects that validation of a message can produce:
   * 1. Record message as validated
   * 2. Record an offence.
   * 3. Trigger validation of unlocked children.
   */
  final case class ValidationEffects[M](
      message: M,
      offenceOpt: Option[Slashing[M]],
      unlockedChildren: List[M]
  )

}
