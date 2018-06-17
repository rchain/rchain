package coop.rchain.casper

import cats.implicits.none
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}

case class EquivocationRecord(
    equivocationBaseBlockSeqNum: SequenceNumber, // This is the sequence number of the equivocator
    equivocationDetectedSeqNum: Map[Validator, SequenceNumber]) // This is the sequence number of the validator

object EquivocationRecord {
  type SequenceNumber = Int
}
