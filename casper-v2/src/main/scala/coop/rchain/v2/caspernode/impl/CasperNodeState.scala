package coop.rchain.v2.caspernode.impl
import cats.effect.IO
import coop.rchain.store.KeyValueTypedStore
import coop.rchain.v2.casper.data.{FinalizationFringe, LatestMessages}
import coop.rchain.v2.casper.stcasper.StateMessage
import coop.rchain.v2.casper.stcasper.syntax.all._
import coop.rchain.v2.casper.{Casper, DependencyGraph, SafetyOracle}
import coop.rchain.v2.caspernode.CasperValidator.ValidationFringe
import coop.rchain.v2.caspernode.impl.CasperNodeState.{casperImpl, CasperMessageData}
import coop.rchain.v2.validation.Offence
import coop.rchain.v2.validation.Offence.Slashing

/**
 * State of Casper node.
 * @param latestMessages      [[LatestMessages]] observed from all known senders.
 * @param justificationsMap   Justifications for a message.
 * @param parentsMap          Parents for a message.
 * @param childrenMap         Children for a message.
 * @param requestedSet        Set of messages Casper requested from the network but not received yet.
 * @param finalizationFringe  [Optional] Finalization fringe.
 * @param validationFringe    Latest validated messages.
 * @param slashings           Slashings, produced by [[Validator]].
 * @param metadataMap         Metadata for a message, required for Casper.
 * @param orderingM
 * @tparam M                  Type for message id.
 * @tparam U                  Type for minimal state unit.
 * @tparam S                  Type for message sender.
 */
case class CasperNodeState[M <: StateMessage[U], U, S](
    latestMessages: LatestMessages[M, S],
    justificationsMap: Map[M, List[M]],
    parentsMap: Map[M, List[M]],
    childrenMap: Map[M, List[M]],
    requestedSet: Set[M],
    finalizationFringe: Option[FinalizationFringe[M]],
    validationFringe: ValidationFringe[M, S],
    slashings: Set[Slashing[M]],
    metadataMap: Map[M, CasperMessageData[M, U, S]]
)(implicit orderingM: Ordering[M]) { self =>

  def observed(
      m: M
  )(seqNum: M => Long, sender: M => S): CasperNodeState[M, U, S] = {
    // Update latest messages
    val newLatestMessages = Casper.updateLatestMessages(latestMessages, m)(seqNum, sender)
    // Remove message from requested set
    val newRequestedSet   = requestedSet - m
    // Recompute finalization fringe, if latest messages are updated
    val newFringe         =
      if (newLatestMessages == latestMessages) finalizationFringe
      else
        casperImpl(self)(metadataMap(_).stateMetadata)
          .computeScope(newLatestMessages, -1)
          .map(_.finalizationFringe)
          .unsafeRunSync() // TODO how to use casper with F in pure code. Make Casper pure?
    self.copy(
      latestMessages = newLatestMessages,
      requestedSet = newRequestedSet,
      finalizationFringe = newFringe
    )
  }

  def validated(m: M, offenceOpt: Option[Offence]) = ???
}

object CasperNodeState {

  /**
   * Properties that Casper requires a message to have. This is to be persisted.
   */
  case class CasperMessageData[M, U, S](
      sender: S,
      seqNum: Int,
      stateMetadata: StateMessage[U],
      bondsMap: Map[S, Long] // Todo this won't change much
  )

  /**
   * Casper instance on [[CasperNodeState]]
   */
  def casperImpl[M, U, S](
      casperNodeState: CasperNodeState[M, U, S],
      complexity: Int = 100
  )(stM: M => StateMessage[U]): Casper[IO, M, S] = {
    val dg = new DependencyGraph[Unit, M, S] {
      override def justifications(message: M): Unit[List[M]] = casperNodeState.justificationsMap _
      override def parents(message: M): Unit[List[M]]        = casperNodeState.parentsMap _
      override def sender(message: M): S                     = casperNodeState.metadataMap(message).sender
      override def seqNum(message: M): Long                  = casperNodeState.metadataMap(message).seqNum
    }
    val so = new SafetyOracle[Unit, M, S] {
      override def compatible(source: M, target: M): Boolean = stM(source).conflicts(stM(target))
      override def bondsMap(message: M): Map[S, Long]        = bondsMap _
    }
    Casper(dg, so, complexity)
  }

  type CasperMessagesStore[F[_], M] = KeyValueTypedStore[F, M, CasperMessageData]
}
