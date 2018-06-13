package coop.rchain.casper

sealed trait BlockStatus
case object Valid         extends BlockStatus
case object InvalidBlock  extends BlockStatus // Invalid blocks that aren't slashable
case object MissingBlocks extends BlockStatus

// The following are slashable
case object InvalidParents        extends BlockStatus
case object InvalidBlockNumber    extends BlockStatus
case object InvalidSender         extends BlockStatus
case object InvalidSequenceNumber extends BlockStatus
case object Equivocation          extends BlockStatus
