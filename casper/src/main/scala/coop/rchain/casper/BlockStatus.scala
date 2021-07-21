package coop.rchain.casper

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash

sealed trait BlockStatus

// Possible outcomes of block processing
object BlockStatus {
  // Ignored, without saving to block storage
  case class Ignored(reason: IgnoreReason) extends BlockStatus
  // Missing dependencies
  case class Incomplete(dependencies: Set[BlockHash]) extends BlockStatus
  // Exception during processing
  final case class BlockException(ex: Throwable) extends BlockStatus

  // Validated successfully
  case class Validated[F[_]](blockMessage: BlockHash, offenceDetected: Option[Offence])
      extends BlockStatus

  // Reasons for block to be ignored
  sealed trait IgnoreReason
  case object InvalidFormat    extends IgnoreReason
  case object InvalidSignature extends IgnoreReason
  case object InvalidSender    extends IgnoreReason
  case object InvalidVersion   extends IgnoreReason
  case object InvalidTimestamp extends IgnoreReason
  case object Known            extends IgnoreReason
  case object Old              extends IgnoreReason

  // Reasons for block to be marked as invalid
  sealed trait Offence
  // AdmissibleEquivocation are blocks that would create an equivocation but are
  // pulled in through a justification of another block
  case object AdmissibleEquivocation extends Offence
  // TODO: Make IgnorableEquivocation slashable again and remember to add an entry to the equivocation record.
  // For now we won't eagerly slash equivocations that we can just ignore,
  // as we aren't forced to add it to our view as a dependency.
  // TODO: The above will become a DOS vector if we don't fix.
  case object IgnorableEquivocation extends Offence

  case object DeployNotSigned         extends Offence
  case object InvalidBlockNumber      extends Offence
  case object InvalidRepeatDeploy     extends Offence
  case object InvalidParents          extends Offence
  case object InvalidFollows          extends Offence
  case object InvalidSequenceNumber   extends Offence
  case object InvalidShardId          extends Offence
  case object JustificationRegression extends Offence
  case object NeglectedInvalidBlock   extends Offence
  case object NeglectedEquivocation   extends Offence
  case object InvalidTransaction      extends Offence
  case object InvalidBondsCache       extends Offence
  case object InvalidBlockHash        extends Offence
  case object InvalidRejectedDeploy   extends Offence
  case object ContainsExpiredDeploy   extends Offence
  case object ContainsFutureDeploy    extends Offence

  val slashableOffenses: Set[Offence] =
    Set(
      AdmissibleEquivocation,
      DeployNotSigned,
      InvalidBlockNumber,
      InvalidRepeatDeploy,
      InvalidParents,
      InvalidFollows,
      InvalidSequenceNumber,
      InvalidShardId,
      JustificationRegression,
      NeglectedInvalidBlock,
      NeglectedEquivocation,
      InvalidTransaction,
      InvalidBondsCache,
      InvalidBlockHash,
      InvalidRejectedDeploy,
      ContainsExpiredDeploy,
      ContainsFutureDeploy
    )

  def isSlashable(invalidBlock: Offence): Boolean =
    slashableOffenses.contains(invalidBlock)
}
