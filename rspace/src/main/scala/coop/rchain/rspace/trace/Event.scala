package coop.rchain.rspace.trace

import coop.rchain.rspace.hashing.StableHashProvider._
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.internal.ConsumeCandidate
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector

import scala.collection.SortedSet

/**
  * Broadly speaking, there are two kinds of events in RSpace,
  *
  *   1. [[IOEvent]]s, which are represented as [[Produce]] and [[Consume]]s
  *   2. [[COMM]] Events, which consist of a single [[Consume]] and one or more [[Produce]]s
  */
sealed trait Event

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

  def apply[C, A](channel: C, datum: A, persistent: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): Produce =
    new Produce(
      hash(channel)(serializeC),
      hash(channel, datum, persistent),
      persistent
    )

  def fromHash(
      channelsHash: Blake2b256Hash,
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Produce =
    new Produce(channelsHash, hash, persistent)
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

object Consume {

  def unapply(arg: Consume): Option[(Seq[Blake2b256Hash], Blake2b256Hash, Int)] =
    Some((arg.channelsHashes, arg.hash, 0))

  def apply[C, P, K](
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

  def fromHash(
      channelsHashes: Seq[Blake2b256Hash],
      hash: Blake2b256Hash,
      persistent: Boolean
  ): Consume =
    new Consume(channelsHashes, hash, persistent)

}
