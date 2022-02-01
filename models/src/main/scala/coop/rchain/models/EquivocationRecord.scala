package coop.rchain.models

import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.EquivocationRecord._
import coop.rchain.models.Validator.Validator

/**
  * A summary of the neglected equivocation algorithm is as follows.
  *
  * Every equivocation has one "base equivocation block" and multiple "children equivocation blocks" where the
  * "children equivocation blocks" have a sequence number that is one greater than the "base equivocation block".
  * To detect neglected equivocations, we keep a set of "equivocation record"s. An "equivocation record" is a tuple
  * containing equivocator's ID, the sequence number of the equivocation base block and a set of block hashes of blocks
  * that point to enough evidence to slash an equivocation corresponding to the "equivocation record".
  * Each time we discover an equivocation, we add a new "equivocation record" entry to the set with the validator's ID
  * and the base equivocation block's sequence number filled in. Each time we add a block to our view,
  * we loop through our "equivocations record"s and see if the block we want to add has enough information to detect
  * the equivocation corresponding to the "equivocation record". There are three cases:
  *
  * Case 1) The block has enough information and the block contains the equivocator in its justification,
  *         we slash the creator of that block
  * Case 2) The block has enough information and the block properly has rotated out the equivocator from its
  *         justification, we update the "equivocation record" so that the set contains this block.
  * Case 3) The block doesn't have enough information and so we do nothing.
  *
  * To ascertain whether a block has enough information to detect a particular equivocation, we loop through the
  * block's justifications and accumulate a set of children equivocation blocks that are reachable from
  * the block's justifications. If at any point while looping through the block's justifications, if we come across a
  * justification block that is in the set of block hashes, we immediately ascertain the block has enough information
  * to detect the equivocation corresponding to the "equivocation record". If at any point the set of children
  * equivocation blocks becomes larger than one in size, we also immediately ascertain the block has enough information
  * to detect the equivocation corresponding to the "equivocation record".
  */
sealed trait EquivocationDiscoveryStatus
case object EquivocationNeglected extends EquivocationDiscoveryStatus
case object EquivocationDetected  extends EquivocationDiscoveryStatus
case object EquivocationOblivious extends EquivocationDiscoveryStatus

// This is the sequence number of the equivocator's base block
final case class EquivocationRecord(
    equivocator: Validator,
    equivocationBaseBlockSeqNum: SequenceNumber,
    equivocationDetectedBlockHashes: Set[BlockHash]
)

object EquivocationRecord {
  type SequenceNumber = Long
}
