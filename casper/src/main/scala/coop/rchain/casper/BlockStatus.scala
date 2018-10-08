package coop.rchain.casper

sealed trait BlockStatus {
  val inDag: Boolean
}

case object Processing extends BlockStatus {
  override val inDag: Boolean = false
}
case class BlockException(ex: Throwable) extends BlockStatus {
  override val inDag: Boolean = false
}

sealed trait ValidBlock extends BlockStatus {
  override val inDag: Boolean = true
}
sealed trait InvalidBlock extends BlockStatus {
  override val inDag: Boolean = true
}
sealed trait Slashable

case object Valid extends ValidBlock

// AdmissibleEquivocation are blocks that would create an equivocation but are
// pulled in through a justification of another block
case object AdmissibleEquivocation extends InvalidBlock with Slashable
// TODO: Make IgnorableEquivocation slashable again and remember to add an entry to the equivocation record.
// For now we won't eagerly slash equivocations that we can just ignore,
// as we aren't forced to add it to our view as a dependency.
// TODO: The above will become a DOS vector if we don't fix.
case object IgnorableEquivocation   extends InvalidBlock
case object InvalidUnslashableBlock extends InvalidBlock
case object MissingBlocks           extends InvalidBlock

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

object BlockStatus {
  def valid: BlockStatus                    = Valid
  def processing: BlockStatus               = Processing
  def exception(ex: Throwable): BlockStatus = BlockException(ex)
}
