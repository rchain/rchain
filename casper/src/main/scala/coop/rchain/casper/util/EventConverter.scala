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
  implicit def byteStringToBlake2b256Hash(hash: ByteString): Blake2b256Hash =
    Blake2b256Hash.fromByteArray(hash.toByteArray)

  implicit def blake2b256HashToByteString(hash: Blake2b256Hash): ByteString =
    ByteString.copyFrom(hash.bytes.toArray)

  def toCasperEvent(event: RspaceEvent): Event = event match {
    case produce: RspaceProduce =>
      Event(Produce(ProduceEvent(produce.channelsHash, produce.hash)))
    case consume: RspaceConsume =>
      Event(Consume(ConsumeEvent(consume.channelsHash, consume.hash)))
    case RspaceComm(rspaceConsume, rspaceProduces) =>
      Event(
        Comm(
          CommEvent(
            Some(ConsumeEvent(rspaceConsume.channelsHash, rspaceConsume.hash)),
            rspaceProduces.map(rspaceProduce =>
              ProduceEvent(rspaceProduce.channelsHash, rspaceProduce.hash))
          )
        ))
  }

  def toRspaceEvent(event: Event): RspaceEvent = event match {
    case Event(Produce(produce: ProduceEvent)) =>
      RspaceProduce.fromHash(produce.channelsHash, produce.hash)
    case Event(Consume(consume: ConsumeEvent)) =>
      RspaceConsume.fromHash(consume.channelsHash, consume.hash)
    case Event(Comm(CommEvent(Some(consume: ConsumeEvent), produces))) =>
      val rspaceProduces: Seq[RspaceProduce] = produces.map { produce =>
        val rspaceProduce: RspaceProduce =
          RspaceProduce.fromHash(produce.channelsHash, produce.hash)
        rspaceProduce
      }.toList
      RspaceComm(RspaceConsume.fromHash(consume.channelsHash, consume.hash), rspaceProduces)
  }
}
