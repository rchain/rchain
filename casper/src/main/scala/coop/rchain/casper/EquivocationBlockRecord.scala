package coop.rchain.casper

import cats.implicits.none
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.InvalidBlockRecord.SequenceNumber

sealed trait EquivocationDiscoveryStatus
case class EquivocationDiscovered(sequenceNumber: SequenceNumber)
    extends EquivocationDiscoveryStatus
case class EquivocationHalfDiscovered(sequenceNumber: SequenceNumber, blockHash: BlockHash)
    extends EquivocationDiscoveryStatus
case object EquivocationOblivious extends EquivocationDiscoveryStatus
case class EquivocationRecord(
    equivocationBaseBlockSeqNum: Option[SequenceNumber],
    equivocationBlockDiscoveryStatuses: Map[Validator, EquivocationDiscoveryStatus])

object EquivocationRecord {
  type SequenceNumber = Int

  def apply(): EquivocationRecord =
    EquivocationRecord(none[SequenceNumber], Map.empty[Validator, EquivocationDiscoveryStatus])
}
