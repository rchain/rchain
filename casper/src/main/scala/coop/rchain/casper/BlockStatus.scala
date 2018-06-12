package coop.rchain.casper

sealed trait BlockStatus
case object Valid                 extends BlockStatus
case object InvalidBlock          extends BlockStatus
case object InvalidSender         extends BlockStatus
case object InvalidBlockSignature extends BlockStatus
case object Equivocation          extends BlockStatus
case object MissingBlocks         extends BlockStatus
case object Undecided             extends BlockStatus
