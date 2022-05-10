package coop.rchain.casper

sealed trait BlockStatus

object BlockStatus {
  def valid: ValidBlock                    = ValidBlock.Valid
  def exception(ex: Throwable): BlockError = BlockError.BlockException(ex)
  def invalidFormat: BlockError            = InvalidBlock.InvalidFormat
  def invalidSender: BlockError            = InvalidBlock.InvalidSender
  def invalidVersion: BlockError           = InvalidBlock.InvalidVersion
  def invalidBlockNumber: BlockError       = InvalidBlock.InvalidBlockNumber
  def invalidRepeatDeploy: BlockError      = InvalidBlock.InvalidRepeatDeploy
  def invalidSequenceNumber: BlockError    = InvalidBlock.InvalidSequenceNumber
  def invalidDeployShardId: BlockError     = InvalidBlock.InvalidDeployShardId
  def justificationRegression: BlockError  = InvalidBlock.JustificationRegression
  def neglectedInvalidBlock: BlockError    = InvalidBlock.NeglectedInvalidBlock
  def invalidTransaction: BlockError       = InvalidBlock.InvalidTransaction
  def invalidBondsCache: BlockError        = InvalidBlock.InvalidBondsCache
  def containsExpiredDeploy: BlockError    = InvalidBlock.ContainsExpiredDeploy
  def containsFutureDeploy: BlockError     = InvalidBlock.ContainsFutureDeploy
  def lowDeployCost: BlockError            = InvalidBlock.LowDeployCost
}

sealed trait ValidBlock extends BlockStatus
object ValidBlock {
  case object Valid extends ValidBlock
}

sealed trait BlockError extends BlockStatus
object BlockError {
  final case class BlockException(ex: Throwable) extends BlockError
}

sealed trait InvalidBlock extends BlockError
object InvalidBlock {
  case object InvalidFormat  extends InvalidBlock
  case object InvalidSender  extends InvalidBlock
  case object InvalidVersion extends InvalidBlock

  case object InvalidBlockNumber      extends InvalidBlock
  case object InvalidRepeatDeploy     extends InvalidBlock
  case object InvalidSequenceNumber   extends InvalidBlock
  case object InvalidDeployShardId    extends InvalidBlock
  case object JustificationRegression extends InvalidBlock
  case object NeglectedInvalidBlock   extends InvalidBlock
  case object InvalidTransaction      extends InvalidBlock
  case object InvalidBondsCache       extends InvalidBlock
  case object InvalidRejectedDeploy   extends InvalidBlock
  case object ContainsExpiredDeploy   extends InvalidBlock
  case object ContainsFutureDeploy    extends InvalidBlock
  case object LowDeployCost           extends InvalidBlock

  val slashableOffenses: Set[InvalidBlock] =
    Set(
      InvalidBlockNumber,
      InvalidRepeatDeploy,
      InvalidSequenceNumber,
      InvalidDeployShardId,
      JustificationRegression,
      NeglectedInvalidBlock,
      InvalidTransaction,
      InvalidBondsCache,
      ContainsExpiredDeploy,
      ContainsFutureDeploy
    )

  def isSlashable(invalidBlock: InvalidBlock): Boolean =
    slashableOffenses.contains(invalidBlock)
}
