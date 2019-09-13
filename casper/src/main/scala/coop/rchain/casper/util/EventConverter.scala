package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{CommEvent, ConsumeEvent, Event, Peek, ProduceEvent}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.trace.{
  COMM => RspaceComm,
  Consume => RspaceConsume,
  Event => RspaceEvent,
  Produce => RspaceProduce
}

import scala.collection.SortedSet

object EventConverter {
  implicit private def byteStringToBlake2b256Hash(hash: ByteString): Blake2b256Hash =
    Blake2b256Hash.fromByteArray(hash.toByteArray)

  implicit private def blake2b256HashToByteString(hash: Blake2b256Hash): ByteString =
    ByteString.copyFrom(hash.bytes.toArray)

  implicit private def blake2b256HashesToByteStrings(
      hashes: List[Blake2b256Hash]
  ): List[ByteString] =
    hashes.map(blake2b256HashToByteString)

  def toCasperEvent(event: RspaceEvent): Event = event match {
    case produce: RspaceProduce =>
      ProduceEvent(
        produce.channelsHash,
        produce.hash,
        produce.persistent,
        produce.sequenceNumber
      )
    case consume: RspaceConsume =>
      ConsumeEvent(
        consume.channelsHashes.toList,
        consume.hash,
        consume.persistent,
        consume.sequenceNumber
      )
    case RspaceComm(rspaceConsume, rspaceProduces, peeks) =>
      CommEvent(
        ConsumeEvent(
          rspaceConsume.channelsHashes.toList,
          rspaceConsume.hash,
          rspaceConsume.persistent,
          rspaceConsume.sequenceNumber
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
          .toList,
        peeks.toList.map(Peek(_))
      )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toRspaceEvent(event: Event): RspaceEvent = event match {
    case ProduceEvent(channelsHash, hash, persistent, sequenceNumber) =>
      RspaceProduce
        .fromHash(channelsHash, hash, persistent, sequenceNumber)
    case ConsumeEvent(channelsHashes, hash, persistent, sequenceNumber) =>
      RspaceConsume.fromHash(
        collection.immutable.Seq(channelsHashes.map(byteStringToBlake2b256Hash): _*),
        hash,
        persistent,
        sequenceNumber
      )
    case CommEvent(consume, produces, peeks) =>
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
        rspaceProduces,
        SortedSet(peeks.map(_.channelIndex): _*)
      )
    case _ => throw new RuntimeException("Could not calculate toRspaceEvent")
  }
}
