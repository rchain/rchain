package coop.rchain.node.casper

import cats.effect.Sync
import coop.rchain.models.Validator.Validator
import coop.rchain.node.casper.CasperNodeState.{CasperMessageData, ObserveEffects, ValidateEffects}
import coop.rchain.v2.casper.data.{FinalizationFringe, LatestMessages}
import coop.rchain.v2.casper.stcasper.StateMessage
import coop.rchain.v2.casper.syntax.all._
import coop.rchain.v2.casper.{Casper, DependencyGraph, SafetyOracle}
import coop.rchain.v2.casperclient.ValidationFringe
import coop.rchain.v2.validation.Offence
import coop.rchain.v2.validation.Offence.{SlashableOffence, Slashing}

/**
 * State of Casper node.
 * @param latestMessages      [[LatestMessages]] observed from all known senders.
 * @param justificationsMap   Justifications for a message.
 * @param parentsMap          Parents for a message.
 * @param childrenMap         Children for a message.
 * @param finalizationFringe  [Optional] Finalization fringe.
 * @param selfValidator       Validator ID of the node itself (if applicable).
 * @param validationFringe    Latest validated messages.
 * @param slashings           Slashings, produced by [[Validator]].
 * @param metadataMap         Metadata for a message, required for Casper.
 * @param requestedSet        Set of messages Casper requested from the network but not received yet.
 * @param processingSet       Set of messages that are currently in process of validation.
 * @param processingSet       Set of messages that are validated.
 * @tparam M                  Type for message id.
 * @tparam U                  Type for minimal state unit.
 * @tparam S                  Type for message sender.
 */
case class CasperNodeState[M: Ordering, U, S](
    // State of the DAG
    latestMessages: LatestMessages[M, S],
    justificationsMap: Map[M, List[M]],
    parentsMap: Map[M, List[M]],
    finalizationFringe: Option[FinalizationFringe[M]],
    selfValidator: Option[S],
    validationFringe: ValidationFringe[M, S],
    slashings: Set[Slashing[M]],
    // Metadata store
    metadataMap: Map[M, CasperMessageData[M, U, S]],
    // State of processing
    requestedSet: Set[M],
    processingSet: Set[M],
    validatedSet: Set[M]
) {

  def observed(
      m: M,
      metadata: CasperMessageData[M, U, S]
  ): (CasperNodeState[M, U, S], ObserveEffects[M, S]) = {
    // Remove message from requested set
    val newRequestedSet      = requestedSet - m
    // Update metadata
    val newMetadataMap       = metadataMap.updated(m, metadata)
    // Update latest messages
    val newLatestMessagesOpt =
      Casper.updateLatestMessages(latestMessages, m)(
        newMetadataMap(_).seqNum,
        newMetadataMap(_).sender
      )
    val missingDeps          = metadata.justifications diff validatedSet diff requestedSet diff processingSet
    val readyForValidation   = (metadata.justifications diff validatedSet).isEmpty
    val runCasper            = true
    (
      copy(
        requestedSet = newRequestedSet,
        metadataMap = newMetadataMap,
        latestMessages = newLatestMessagesOpt.getOrElse(latestMessages)
      ),
      ObserveEffects(m, newLatestMessagesOpt, runCasper, readyForValidation, missingDeps)
    )
  }

  def validated(
      m: M,
      offenceOpt: Option[Offence]
  ): (CasperNodeState[M, U, S], ValidateEffects[M]) = {
    val slashingOpt         = offenceOpt.collect { case so: SlashableOffence =>
      Slashing(m, so)
    }
    val newSlashings        = slashingOpt.map(slashings + _).getOrElse(slashings)
    val newValidationFringe =
      Casper
        .updateLatestMessages(validationFringe, m)(metadataMap(_).seqNum, metadataMap(_).sender)
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
      ValidateEffects(m, slashingOpt, unlockedChildren)
    )
  }
}

object CasperNodeState {

  /**
   * Effects that observing a message might produce:
   * 1. Add message to the state.
   * 2. Update latest messages.
   * 3. Update finalization fringe.
   * 4. Forward message to validation if all dependencies are already validated.
   * 5. Request missing dependencies if there are some.
   */
  case class ObserveEffects[M, S](
      message: M,
      newLatestMessages: Option[LatestMessages[M, S]],
      runCasper: Boolean, //Todo can we observe another message while casper run requested from previous add is running?
      forwardToValidator: Boolean,
      dependenciesMissing: Set[M]
  )

  /**
   * Effects that validation of a message can produce:
   * 1. Record message as validated
   * 2. Record an offence.
   * 3. Trigger validation of unlocked children.
   */
  final case class ValidateEffects[M](
      message: M,
      offenceOpt: Option[Slashing[M]],
      unlockedChildren: List[M]
  )

  /**
   * Properties that Casper requires a message to have. This is to be persisted.
   */
  case class CasperMessageData[M, U, S](
      justifications: Set[M],
      sender: S,
      seqNum: Int,
      stateMetadata: StateMessage[U],
      bondsMap: Map[S, Long] // Todo this won't change much
  )

  /**
   * Casper instance on [[CasperNodeState]]
   */
  def casperImpl[F[_]: Sync, M, U, S](
      casperNodeState: CasperNodeState[M, U, S],
      complexity: Int = 100
  )(stM: M => StateMessage[U]): Casper[F, M, S] = {
    val dg = new DependencyGraph[F, M, S] {
      override def justifications(message: M): F[List[M]] =
        Sync[F].delay(casperNodeState.justificationsMap _)
      override def parents(message: M): F[List[M]]        = Sync[F].delay(casperNodeState.parentsMap _)
      override def sender(message: M): S                  = casperNodeState.metadataMap(message).sender
      override def seqNum(message: M): Long               = casperNodeState.metadataMap(message).seqNum
    }
    val so = new SafetyOracle[Unit, M, S] {
      override def compatible(source: M, target: M): Boolean = stM(source).conflicts(stM(target))
      override def bondsMap(message: M): Map[S, Long]        = bondsMap _
    }
    Casper(dg, so, complexity)
  }
}
