package coop.rchain.casper

sealed trait BlockStatus
case object Valid                   extends BlockStatus
case object InvalidUnslashableBlock extends BlockStatus
case object MissingBlocks           extends BlockStatus

case object InvalidBlockNumber      extends BlockStatus
case object InvalidParents          extends BlockStatus
case object InvalidSequenceNumber   extends BlockStatus
case object JustificationRegression extends BlockStatus // Note equivocations will be caught before justification regressions
case object Equivocation            extends BlockStatus
