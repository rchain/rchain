package coop.rchain.rspace.trace

import cats.effect.Sync
import coop.rchain.rspace.StableHashProvider._
import coop.rchain.rspace.internal._
import cats.implicits._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.Serialize
import coop.rchain.rspace.internal.codecSeq
import coop.rchain.shared.Serialize
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
    peeks: SortedSet[Int],
    timesRepeated: Map[Produce, Int]
) extends Event

object COMM {
  def apply[C, A](
      dataCandidates: Seq[ConsumeCandidate[C, A]],
      consumeRef: Consume,
      peeks: SortedSet[Int],
      produceCounters: (Seq[Produce]) => Map[Produce, Int]
  ): COMM = {
    val produceRefs =
      dataCandidates.map(_.datum.source).sortBy(p => (p.channelsHash, p.hash, p.persistent))

    COMM(consumeRef, produceRefs, peeks, produceCounters(produceRefs))
  }
  implicit val codecInt = int32
  implicit val codecCOMM: Codec[COMM] =
    (Codec[Consume] :: Codec[Seq[Produce]] :: sortedSet(uint8) :: Codec[Map[Produce, Int]]).as[COMM]
}

sealed trait IOEvent extends Event

final case class Produce private (
    channelsHash: Blake2b256Hash,
    hash: Blake2b256Hash,
    persistent: Boolean
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47

  override def toString: String =
    s"Produce(channels: ${channelsHash.toString}, hash: ${hash.toString})"

}

object Produce {

  def create[C, A](channel: C, datum: A, persistent: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): Produce =
    new Produce(
      hash(channel)(serializeC),
      hash(channel, datum, persistent),
      persistent
    )

  def createF[F[_]: Sync, C, A](channel: C, datum: A, persistent: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): F[Produce] = Sync[F].delay(create(channel, datum, persistent))

  def fromHash(
      channelsHash: Blake2b256Hash,
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Produce =
    new Produce(channelsHash, hash, persistent)

  implicit val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: bool).as[Produce]
}

final case class Consume private (
    channelsHashes: Seq[Blake2b256Hash],
    hash: Blake2b256Hash,
    persistent: Boolean
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode() * 47

  override def toString: String =
    s"Consume(channels: ${channelsHashes.toString}, hash: ${hash.toString}, persistent: $persistent)"
}

final case class EventGroup(produces: Set[Produce], consumes: Set[Consume], comms: Set[COMM]) {
  def events: Seq[Event] = produces.toSeq ++ consumes.toSeq ++ comms.toSeq
}

object Consume {

  def unapply(arg: Consume): Option[(Seq[Blake2b256Hash], Blake2b256Hash, Int)] =
    Some((arg.channelsHashes, arg.hash, 0))

  def create[C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persistent: Boolean
  )(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): Consume = {
    val channelsByteVectors: Seq[ByteVector] = toOrderedByteVectors(channels)
    new Consume(
      channelsByteVectors.map(Blake2b256Hash.create),
      hash(channelsByteVectors, patterns, continuation, persistent),
      persistent
    )
  }

  def createF[F[_]: Sync, C, P, K](
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persistent: Boolean
  )(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): F[Consume] = Sync[F].delay(create(channels, patterns, continuation, persistent))

  def fromHash(
      channelsHashes: Seq[Blake2b256Hash],
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Consume =
    new Consume(channelsHashes, hash, persistent)

  implicit val codecConsume: Codec[Consume] =
    (Codec[Seq[Blake2b256Hash]] :: Codec[Blake2b256Hash] :: bool).as[Consume]

  def hasJoins(consume: Consume): Boolean = consume.channelsHashes.size > 1
}
