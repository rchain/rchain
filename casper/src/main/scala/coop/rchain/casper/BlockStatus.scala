package coop.rchain.casper

sealed trait BlockStatus
// TODO: Rename
sealed trait IncludeableBlock extends BlockStatus
sealed trait RejectableBlock  extends BlockStatus
sealed trait Slashable

case object Valid extends IncludeableBlock
// AdmissibleEquivocation are blocks that would create an equivocation but are
// pulled in through a justification of another block
case object AdmissibleEquivocation extends IncludeableBlock with Slashable
// TODO: Make IgnorableEquivocation slashable again and remember to add an entry to the equivocation record.
// We will also need to add a new "equivocation child" field to the BlockMessage proto
// and check for unnecessary inclusions of a equivocation child.
// For now we won't eagerly slash equivocations that we can just ignore,
// as we aren't forced to add it to our view as a dependency.
case object IgnorableEquivocation   extends RejectableBlock
case object InvalidUnslashableBlock extends RejectableBlock
case object MissingBlocks           extends RejectableBlock

case object InvalidBlockNumber      extends RejectableBlock with Slashable
case object InvalidParents          extends RejectableBlock with Slashable
case object InvalidSequenceNumber   extends RejectableBlock with Slashable
case object JustificationRegression extends RejectableBlock with Slashable
case object NeglectedInvalidBlock   extends RejectableBlock with Slashable
case object NeglectedEquivocation   extends RejectableBlock with Slashable
case object InvalidTransaction      extends RejectableBlock with Slashable
