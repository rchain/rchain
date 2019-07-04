package coop.rchain.rspace
package experiment
package trace

import coop.rchain.rspace.StableHashProvider._
import internal._
import cats.implicits._
import coop.rchain.rspace.internal.codecSeq
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

import scala.collection.SortedSet

/**
  * Broadly speaking, there are two kinds of events in RSpace,
  *
  *   1. [[IOEvent]]s, which are represented as [[Produce]] and [[Consume]]s
  *   2. [[COMM]] Events, which consist of a single [[Consume]] and one or more [[Produce]]s
  */
sealed trait Event

object Event {

  implicit def codecEvent: Codec[Event] =
    discriminated[Event]
      .by(uint2)
      .subcaseP(0) {
        case (comm: COMM) => comm
      }(Codec[COMM])
      .subcaseP(1) {
        case produce: Produce => produce
      }(Codec[Produce])
      .subcaseP(2) {
        case consume: Consume => consume
      }(Codec[Consume])

  implicit def codecLog: Codec[Seq[Event]] = codecSeq[Event](codecEvent)
}

final case class COMM(
    consume: Consume,
    produces: Seq[Produce],
    peeks: SortedSet[Int] = SortedSet.empty
) extends Event

object COMM {
  implicit val codecCOMM: Codec[COMM] =
    (Codec[Consume] :: Codec[Seq[Produce]] :: sortedSet(uint8)).as[COMM]
}

sealed trait IOEvent extends Event

final case class Produce private (
    channelsHash: Blake2b256Hash,
    hash: Blake2b256Hash,
    sequenceNumber: Int
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash && produce.sequenceNumber == sequenceNumber
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47 + sequenceNumber.hashCode()

  override def toString: String =
    s"Produce(channels: ${channelsHash.toString}, hash: ${hash.toString})"

}

object Produce {

  def unapply(arg: Produce): Option[(Blake2b256Hash, Blake2b256Hash, Int)] =
    Some((arg.channelsHash, arg.hash, arg.sequenceNumber))

  def create[C, A](channel: C, datum: A, persist: Boolean, sequenceNumber: Int = 0)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): Produce =
    new Produce(
      hash(channel)(serializeC),
      hash(channel, datum, persist),
      sequenceNumber
    )

  def fromHash(channelsHash: Blake2b256Hash, hash: Blake2b256Hash, sequenceNumber: Int): Produce =
    new Produce(channelsHash, hash, sequenceNumber)

  implicit val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: int32).as[Produce]
}

final case class Consume private (
    channelsHashes: Seq[Blake2b256Hash],
    hash: Blake2b256Hash
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47

  override def toString: String =
    s"Consume(channels: ${channelsHashes.toString}, hash: ${hash.toString})"
}

object Consume {

  def unapply(arg: Consume): Option[(Seq[Blake2b256Hash], Blake2b256Hash)] =
    Some((arg.channelsHashes, arg.hash))

  def create[C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean
  )(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): Consume = {
    val channelsByteVectors: Seq[ByteVector] = toOrderedByteVectors(channels)
    new Consume(
      channelsByteVectors.map(Blake2b256Hash.create),
      hash(channelsByteVectors, patterns, continuation, persist)
    )
  }

  def fromHash(
      channelsHashes: Seq[Blake2b256Hash],
      hash: Blake2b256Hash,
      sequenceNumber: Int
  ): Consume =
    new Consume(channelsHashes, hash)

  implicit val codecConsume: Codec[Consume] =
    (Codec[Seq[Blake2b256Hash]] :: Codec[Blake2b256Hash]).as[Consume]
}
