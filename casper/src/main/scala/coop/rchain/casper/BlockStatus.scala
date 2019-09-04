package coop.rchain.casper

sealed trait BlockStatus {
  val inDag: Boolean
}

sealed trait BlockError extends BlockStatus

sealed trait InvalidBlock extends BlockError {
  override val inDag: Boolean = true
}

sealed trait Slashable

sealed trait ValidBlock extends BlockStatus {
  override val inDag: Boolean = true
}

case object Valid extends ValidBlock

case object Processing extends BlockStatus {
  override val inDag: Boolean = false
}

final case class BlockException(ex: Throwable) extends BlockError {
  override val inDag: Boolean = false
}

// AdmissibleEquivocation are blocks that would create an equivocation but are
// pulled in through a justification of another block
case object AdmissibleEquivocation extends InvalidBlock with Slashable
// TODO: Make IgnorableEquivocation slashable again and remember to add an entry to the equivocation record.
// For now we won't eagerly slash equivocations that we can just ignore,
// as we aren't forced to add it to our view as a dependency.
// TODO: The above will become a DOS vector if we don't fix.
case object IgnorableEquivocation extends InvalidBlock
case object MissingBlocks         extends InvalidBlock

sealed trait InvalidUnslashableBlock extends InvalidBlock
case object InvalidFormat            extends InvalidUnslashableBlock
case object InvalidSignature         extends InvalidUnslashableBlock
case object InvalidSender            extends InvalidUnslashableBlock
case object InvalidVersion           extends InvalidUnslashableBlock
case object InvalidTimestamp         extends InvalidUnslashableBlock

case object InvalidBlockNumber      extends InvalidBlock with Slashable
case object InvalidRepeatDeploy     extends InvalidBlock with Slashable
case object InvalidParents          extends InvalidBlock with Slashable
case object InvalidFollows          extends InvalidBlock with Slashable
case object InvalidSequenceNumber   extends InvalidBlock with Slashable
case object InvalidShardId          extends InvalidBlock with Slashable
case object JustificationRegression extends InvalidBlock with Slashable
case object NeglectedInvalidBlock   extends InvalidBlock with Slashable
case object NeglectedEquivocation   extends InvalidBlock with Slashable
case object InvalidTransaction      extends InvalidBlock with Slashable
case object InvalidBondsCache       extends InvalidBlock with Slashable
case object InvalidBlockHash        extends InvalidBlock with Slashable
case object InvalidDeployCount      extends InvalidBlock with Slashable
case object ContainsExpiredDeploy   extends InvalidBlock with Slashable
case object ContainsFutureDeploy    extends InvalidBlock with Slashable

object BlockStatus {
  def valid: ValidBlock                    = Valid
  def processing: BlockStatus              = Processing
  def exception(ex: Throwable): BlockError = BlockException(ex)
  def admissibleEquivocation: BlockError   = AdmissibleEquivocation
  def ignorableEquivocation: BlockError    = IgnorableEquivocation
  def missingBlocks: BlockError            = MissingBlocks
  def invalidFormat: BlockError            = InvalidFormat
  def invalidSignature: BlockError         = InvalidSignature
  def invalidSender: BlockError            = InvalidSender
  def invalidVersion: BlockError           = InvalidVersion
  def invalidTimestamp: BlockError         = InvalidTimestamp
  def invalidBlockNumber: BlockError       = InvalidBlockNumber
  def invalidRepeatDeploy: BlockError      = InvalidRepeatDeploy
  def invalidParents: BlockError           = InvalidParents
  def invalidFollows: BlockError           = InvalidFollows
  def invalidSequenceNumber: BlockError    = InvalidSequenceNumber
  def invalidShardId: BlockError           = InvalidShardId
  def justificationRegression: BlockError  = JustificationRegression
  def neglectedInvalidBlock: BlockError    = NeglectedInvalidBlock
  def neglectedEquivocation: BlockError    = NeglectedEquivocation
  def invalidTransaction: BlockError       = InvalidTransaction
  def invalidBondsCache: BlockError        = InvalidBondsCache
  def invalidBlockHash: BlockError         = InvalidBlockHash
  def invalidDeployCount: BlockError       = InvalidDeployCount
  def containsExpiredDeploy: BlockError    = ContainsExpiredDeploy
  def containsFutureDeploy: BlockError     = ContainsFutureDeploy
}
