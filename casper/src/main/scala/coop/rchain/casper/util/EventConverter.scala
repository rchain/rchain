package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{CommEvent, ConsumeEvent, Event, ProduceEvent}
import coop.rchain.casper.protocol.Event.EventInstance.{Comm, Consume, Produce}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace.{
  COMM => RspaceComm,
  Consume => RspaceConsume,
  Event => RspaceEvent,
  Produce => RspaceProduce
}
import scala.collection.immutable.Seq

object EventConverter {
  def toCasperEvent(event: RspaceEvent): Event = event match {
    case RspaceProduce(produce) =>
      Event(Produce(ProduceEvent(ByteString.copyFrom(produce.bytes.toArray))))
    case RspaceConsume(consume) =>
      Event(Consume(ConsumeEvent(ByteString.copyFrom(consume.bytes.toArray))))
    case RspaceComm(rspaceConsume, rspaceProduces) =>
      Event(
        Comm(
          CommEvent(
            Some(ConsumeEvent(ByteString.copyFrom(rspaceConsume.hash.bytes.toArray))),
            rspaceProduces.map(rspaceProduce =>
              ProduceEvent(ByteString.copyFrom(rspaceProduce.hash.bytes.toArray)))
          )))
  }

  def toRspaceEvent(event: Event): RspaceEvent = event match {
    case Event(Produce(ProduceEvent(produce))) =>
      RspaceProduce.fromHash(Blake2b256Hash.fromByteArray(produce.toByteArray))
    case Event(Consume(ConsumeEvent(consume))) =>
      RspaceConsume.fromHash(Blake2b256Hash.fromByteArray(consume.toByteArray))
    case Event(Comm(CommEvent(Some(ConsumeEvent(consume)), produces))) =>
      val rspaceProduces: Seq[RspaceProduce] = produces.map {
        case ProduceEvent(produce) =>
          val rspaceProduce: RspaceProduce =
            RspaceProduce.fromHash(Blake2b256Hash.fromByteArray(produce.toByteArray))
          rspaceProduce
      }.toList
      RspaceComm(RspaceConsume.fromHash(Blake2b256Hash.fromByteArray(consume.toByteArray)),
                 rspaceProduces)
  }
}
