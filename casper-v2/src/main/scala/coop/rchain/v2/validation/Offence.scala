package coop.rchain.v2.validation

/**
 * Violation of Casper rules.
 */
trait Offence

object Offence {

  /**
   * Offence that lead to slashing of offender.
   */
  trait SlashableOffence extends Offence

  final case class Slashing[M](message: M, offence: SlashableOffence)

  /**
   * Message should have justifications for validators mentioned in message scope:
   * all slashed, all bonded.
   */
  case object IncompleteJustification

  /**
   * Sender created message which does not have previous message in justification.
   *    *   *   *
   *    | /   /
   *    |/  /
   *    * * <- Sender 1 does sequence equivocation.
   *    \/
   *    *
   *    1   2   3
   */
  case object SequenceEquivocation

  /**
   * Sender created message which does not agree on self justification.
   */
  case object SelfDisagreement

  //  /** Sender create two messages that are not compatible with each other */
  //  case object Equivocation extends Offence Todo cleanup

  case object DummyOffence extends SlashableOffence // TODO remove and store concrete Offence
  case object Equivocation extends SlashableOffence

  case object AdmissibleEquivocation  extends SlashableOffence
  case object IgnorableEquivocation   extends SlashableOffence
  case object InvalidFormat           extends SlashableOffence
  case object InvalidSignature        extends SlashableOffence
  case object InvalidSender           extends SlashableOffence
  case object InvalidVersion          extends SlashableOffence
  case object InvalidTimestamp        extends SlashableOffence
  case object DeployNotSigned         extends SlashableOffence
  case object InvalidBlockNumber      extends SlashableOffence
  case object InvalidRepeatDeploy     extends SlashableOffence
  case object InvalidParents          extends SlashableOffence
  case object InvalidFollows          extends SlashableOffence
  case object InvalidSequenceNumber   extends SlashableOffence
  case object InvalidShardId          extends SlashableOffence
  case object JustificationRegression extends SlashableOffence
  case object NeglectedInvalidBlock   extends SlashableOffence
  case object NeglectedEquivocation   extends SlashableOffence
  case object InvalidTransaction      extends SlashableOffence
  case object InvalidMessageScope     extends SlashableOffence
  case object InvalidBondsCache       extends SlashableOffence
  case object InvalidBlockHash        extends SlashableOffence
  case object ContainsExpiredDeploy   extends SlashableOffence
  case object ContainsFutureDeploy    extends SlashableOffence
  case object InvalidRejectedDeploy   extends SlashableOffence
  case object NotOfInterest           extends SlashableOffence

  def admissibleEquivocation: Offence  = AdmissibleEquivocation
  def ignorableEquivocation: Offence   = IgnorableEquivocation
  def invalidFormat: Offence           = InvalidFormat
  def invalidSignature: Offence        = InvalidSignature
  def invalidSender: Offence           = InvalidSender
  def invalidVersion: Offence          = InvalidVersion
  def invalidTimestamp: Offence        = InvalidTimestamp
  def deployNotSigned: Offence         = DeployNotSigned
  def invalidBlockNumber: Offence      = InvalidBlockNumber
  def invalidRepeatDeploy: Offence     = InvalidRepeatDeploy
  def invalidParents: Offence          = InvalidParents
  def invalidFollows: Offence          = InvalidFollows
  def invalidSequenceNumber: Offence   = InvalidSequenceNumber
  def invalidShardId: Offence          = InvalidShardId
  def justificationRegression: Offence = JustificationRegression
  def neglectedInvalidBlock: Offence   = NeglectedInvalidBlock
  def neglectedEquivocation: Offence   = NeglectedEquivocation
  def invalidTransaction: Offence      = InvalidTransaction
  def invalidMessageScope: Offence     = InvalidMessageScope
  def invalidBondsCache: Offence       = InvalidBondsCache
  def invalidBlockHash: Offence        = InvalidBlockHash
  def containsExpiredDeploy: Offence   = ContainsExpiredDeploy
  def containsFutureDeploy: Offence    = ContainsFutureDeploy
  def invalidRejectedDeploy: Offence   = InvalidRejectedDeploy
  def notOfInterest: Offence           = NotOfInterest
}
