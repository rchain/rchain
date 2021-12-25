package coop.rchain.casper

sealed trait BlockStatus
object BlockStatus {
  def valid: ValidBlock                    = ValidBlock.Valid
  def processed: BlockError                = BlockError.Processed
  def casperIsBusy: BlockError             = BlockError.CasperIsBusy
  def exception(ex: Throwable): BlockError = BlockError.BlockException(ex)
  def missingBlocks: BlockError            = BlockError.MissingBlocks
  def admissibleEquivocation: BlockError   = InvalidBlock.AdmissibleEquivocation
  def ignorableEquivocation: BlockError    = InvalidBlock.IgnorableEquivocation
  def invalidFormat: BlockError            = InvalidBlock.InvalidFormat
  def invalidSignature: BlockError         = InvalidBlock.InvalidSignature
  def invalidSender: BlockError            = InvalidBlock.InvalidSender
  def invalidVersion: BlockError           = InvalidBlock.InvalidVersion
  def invalidTimestamp: BlockError         = InvalidBlock.InvalidTimestamp
  def deployNotSigned: BlockError          = InvalidBlock.DeployNotSigned
  def invalidBlockNumber: BlockError       = InvalidBlock.InvalidBlockNumber
  def invalidRepeatDeploy: BlockError      = InvalidBlock.InvalidRepeatDeploy
  def invalidParents: BlockError           = InvalidBlock.InvalidParents
  def invalidFollows: BlockError           = InvalidBlock.InvalidFollows
  def invalidSequenceNumber: BlockError    = InvalidBlock.InvalidSequenceNumber
  def invalidShardId: BlockError           = InvalidBlock.InvalidShardId
  def justificationRegression: BlockError  = InvalidBlock.JustificationRegression
  def neglectedInvalidBlock: BlockError    = InvalidBlock.NeglectedInvalidBlock
  def neglectedEquivocation: BlockError    = InvalidBlock.NeglectedEquivocation
  def invalidTransaction: BlockError       = InvalidBlock.InvalidTransaction
  def invalidBondsCache: BlockError        = InvalidBlock.InvalidBondsCache
  def invalidBlockHash: BlockError         = InvalidBlock.InvalidBlockHash
  def containsExpiredDeploy: BlockError    = InvalidBlock.ContainsExpiredDeploy
  def containsFutureDeploy: BlockError     = InvalidBlock.ContainsFutureDeploy
  def notOfInterest: BlockError            = InvalidBlock.NotOfInterest
  def lowDeployCost: BlockError            = InvalidBlock.LowDeployCost

  def isInDag(blockStatus: BlockStatus): Boolean =
    blockStatus match {
      case _: ValidBlock   => true
      case _: InvalidBlock => true
      case _               => false
    }
}

sealed trait ValidBlock extends BlockStatus
object ValidBlock {
  case object Valid extends ValidBlock
}

sealed trait BlockError extends BlockStatus
object BlockError {
  case object Processed                          extends BlockError
  case object CasperIsBusy                       extends BlockError
  case object MissingBlocks                      extends BlockError
  final case class BlockException(ex: Throwable) extends BlockError
}

sealed trait InvalidBlock extends BlockError
object InvalidBlock {
  // AdmissibleEquivocation are blocks that would create an equivocation but are
  // pulled in through a justification of another block
  case object AdmissibleEquivocation extends InvalidBlock
  // TODO: Make IgnorableEquivocation slashable again and remember to add an entry to the equivocation record.
  // For now we won't eagerly slash equivocations that we can just ignore,
  // as we aren't forced to add it to our view as a dependency.
  // TODO: The above will become a DOS vector if we don't fix.
  case object IgnorableEquivocation extends InvalidBlock

  case object InvalidFormat    extends InvalidBlock
  case object InvalidSignature extends InvalidBlock
  case object InvalidSender    extends InvalidBlock
  case object InvalidVersion   extends InvalidBlock
  case object InvalidTimestamp extends InvalidBlock

  case object DeployNotSigned         extends InvalidBlock
  case object InvalidBlockNumber      extends InvalidBlock
  case object InvalidRepeatDeploy     extends InvalidBlock
  case object InvalidParents          extends InvalidBlock
  case object InvalidFollows          extends InvalidBlock
  case object InvalidSequenceNumber   extends InvalidBlock
  case object InvalidShardId          extends InvalidBlock
  case object JustificationRegression extends InvalidBlock
  case object NeglectedInvalidBlock   extends InvalidBlock
  case object NeglectedEquivocation   extends InvalidBlock
  case object InvalidTransaction      extends InvalidBlock
  case object InvalidBondsCache       extends InvalidBlock
  case object InvalidBlockHash        extends InvalidBlock
  case object InvalidRejectedDeploy   extends InvalidBlock
  case object ContainsExpiredDeploy   extends InvalidBlock
  case object ContainsFutureDeploy    extends InvalidBlock
  case object NotOfInterest           extends InvalidBlock
  case object LowDeployCost           extends InvalidBlock

  val slashableOffenses: Set[InvalidBlock] =
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
      ContainsExpiredDeploy,
      ContainsFutureDeploy
    )

  def isSlashable(invalidBlock: InvalidBlock): Boolean =
    slashableOffenses.contains(invalidBlock)
}
