package coop.rchain.casper

import cats.implicits.none
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}

sealed trait EquivocationDiscoveryStatus
case object EquivocationNeglected extends EquivocationDiscoveryStatus
case object EquivocationDetected  extends EquivocationDiscoveryStatus
case object EquivocationOblivious extends EquivocationDiscoveryStatus

// This is the sequence number of the equivocator
case class EquivocationRecord(equivocator: Validator,
                              equivocationBaseBlockSeqNum: SequenceNumber,
                              equivocationDetectedBlockHashes: Set[BlockHash])

object EquivocationRecord {
  type SequenceNumber = Int
}
