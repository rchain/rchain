package coop.rchain.casper

import cats.implicits.none
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.InvalidBlockRecord.SequenceNumber

case class InvalidBlockRecord(invalidBlockSeqNum: Option[SequenceNumber],
                              invalidBlockDiscoveryStatuses: Map[Validator, Option[SequenceNumber]])
object InvalidBlockRecord {
  type SequenceNumber = Int

  def apply(): InvalidBlockRecord =
    InvalidBlockRecord(none[SequenceNumber], Map.empty[Validator, Option[SequenceNumber]])
}
