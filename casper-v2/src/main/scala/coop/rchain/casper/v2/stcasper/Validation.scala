package coop.rchain.casper.v2.stcasper
import coop.rchain.casper.v2.core.Validation.{Offence, SlashableOffence}

object Validation {

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
