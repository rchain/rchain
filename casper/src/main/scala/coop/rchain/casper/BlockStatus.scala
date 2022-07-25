package coop.rchain.casper

sealed trait BlockStatus

object BlockStatus {
  def valid: ValidBlock                     = ValidBlock.Valid
  def invalidBlockNumber: InvalidBlock      = InvalidBlock.InvalidBlockNumber
  def invalidRepeatDeploy: InvalidBlock     = InvalidBlock.InvalidRepeatDeploy
  def invalidSequenceNumber: InvalidBlock   = InvalidBlock.InvalidSequenceNumber
  def invalidDeployShardId: InvalidBlock    = InvalidBlock.InvalidDeployShardId
  def justificationRegression: InvalidBlock = InvalidBlock.JustificationRegression
  def neglectedInvalidBlock: InvalidBlock   = InvalidBlock.NeglectedInvalidBlock
  def invalidStateHash: InvalidBlock        = InvalidBlock.InvalidStateHash
  def invalidBondsCache: InvalidBlock       = InvalidBlock.InvalidBondsCache
  def containsExpiredDeploy: InvalidBlock   = InvalidBlock.ContainsExpiredDeploy
  def containsFutureDeploy: InvalidBlock    = InvalidBlock.ContainsFutureDeploy
  def containsLowCostDeploy: InvalidBlock   = InvalidBlock.ContainsLowCostDeploy
}

sealed trait ValidBlock extends BlockStatus
object ValidBlock {
  case object Valid extends ValidBlock
}

sealed trait InvalidBlock extends BlockStatus

object InvalidBlock {
  case object InvalidBlockNumber      extends InvalidBlock
  case object InvalidRepeatDeploy     extends InvalidBlock
  case object InvalidSequenceNumber   extends InvalidBlock
  case object InvalidDeployShardId    extends InvalidBlock
  case object JustificationRegression extends InvalidBlock
  case object NeglectedInvalidBlock   extends InvalidBlock
  case object InvalidStateHash        extends InvalidBlock
  case object InvalidBondsCache       extends InvalidBlock
  case object InvalidRejectedDeploy   extends InvalidBlock
  case object ContainsExpiredDeploy   extends InvalidBlock
  case object ContainsFutureDeploy    extends InvalidBlock
  case object ContainsLowCostDeploy   extends InvalidBlock
}
