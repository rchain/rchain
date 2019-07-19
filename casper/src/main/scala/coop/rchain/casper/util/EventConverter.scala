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

object EventConverter {
  implicit private def byteStringToBlake2b256Hash(hash: ByteString): Blake2b256Hash =
    Blake2b256Hash.fromByteArray(hash.toByteArray)

  implicit private def blake2b256HashToByteString(hash: Blake2b256Hash): ByteString =
    ByteString.copyFrom(hash.bytes.toArray)

  implicit private def blake2b256HashesToByteStrings(hashes: Seq[Blake2b256Hash]): Seq[ByteString] =
    hashes.map(blake2b256HashToByteString)

  def toCasperEvent(event: RspaceEvent): Event = event match {
    case produce: RspaceProduce =>
      Event(
        Produce(
          ProduceEvent(
            produce.channelsHash,
            produce.hash,
            produce.persistent,
            produce.sequenceNumber
          )
        )
      )
    case consume: RspaceConsume =>
      Event(
        Consume(
          ConsumeEvent(
            consume.channelsHashes,
            consume.hash,
            consume.persistent,
            consume.sequenceNumber
          )
        )
      )
    case RspaceComm(rspaceConsume, rspaceProduces, _) => // TODO address peek
      Event(
        Comm(
          CommEvent(
            Some(
              ConsumeEvent(
                rspaceConsume.channelsHashes,
                rspaceConsume.hash,
                rspaceConsume.persistent,
                rspaceConsume.sequenceNumber
              )
            ),
            rspaceProduces
              .map(
                rspaceProduce =>
                  ProduceEvent(
                    rspaceProduce.channelsHash,
                    rspaceProduce.hash,
                    rspaceProduce.persistent,
                    rspaceProduce.sequenceNumber
                  )
              )
          )
        )
      )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toRspaceEvent(event: Event): RspaceEvent = event match {
    case Event(Produce(produce: ProduceEvent)) =>
      RspaceProduce
        .fromHash(produce.channelsHash, produce.hash, produce.persistent, produce.sequenceNumber)
    case Event(Consume(consume: ConsumeEvent)) =>
      RspaceConsume.fromHash(
        collection.immutable.Seq(consume.channelsHashes.map(byteStringToBlake2b256Hash): _*),
        consume.hash,
        consume.persistent,
        consume.sequenceNumber
      )
    case Event(Comm(CommEvent(Some(consume: ConsumeEvent), produces))) =>
      val rspaceProduces: Seq[RspaceProduce] = produces.map { produce =>
        val rspaceProduce: RspaceProduce =
          RspaceProduce.fromHash(
            produce.channelsHash,
            produce.hash,
            produce.persistent,
            produce.sequenceNumber
          )
        rspaceProduce
      }.toList
      RspaceComm(
        RspaceConsume.fromHash(
          collection.immutable.Seq(consume.channelsHashes.map(byteStringToBlake2b256Hash): _*),
          consume.hash,
          consume.persistent,
          consume.sequenceNumber
        ),
        rspaceProduces
      )
    case _ => throw new RuntimeException("Could not calculate toRspaceEvent")
  }
}
