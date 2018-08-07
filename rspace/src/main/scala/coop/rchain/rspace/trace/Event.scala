package coop.rchain.rspace.trace

import coop.rchain.rspace.StableHashProvider
import cats.implicits._
import coop.rchain.rspace.{Blake2b256Hash, Serialize}
import coop.rchain.rspace.internal.codecSeq
import scala.collection.immutable.Seq
import scodec.Codec
import scodec.codecs._

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
      .by(uint8)
      .subcaseP(0) {
        case (comm: COMM) => comm
      }(Codec[COMM])
      .subcaseP(1) {
        case (produce: Produce) => produce
      }(Codec[Produce])
      .subcaseP(2) {
        case (consume: Consume) => consume
      }(Codec[Consume])

  implicit def codecLog: Codec[Seq[Event]] = codecSeq[Event](codecEvent)
}

case class COMM(consume: Consume, produces: Seq[Produce]) extends Event

object COMM {

  implicit val codecCOMM: Codec[COMM] = (Codec[Consume] :: Codec[Seq[Produce]]).as[COMM]
}

sealed trait IOEvent extends Event

case class Produce private (channelsHash: Blake2b256Hash, hash: Blake2b256Hash) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String =
    s"Produce(channels: ${channelsHash.toString}, hash: ${hash.toString})"

}

object Produce {

  def unapply(arg: Produce): Option[(Blake2b256Hash, Blake2b256Hash)] =
    Some((arg.channelsHash, arg.hash))

  def create[C, A](channel: C, datum: A, persist: Boolean)(implicit
                                                           serializeC: Serialize[C],
                                                           serializeA: Serialize[A]): Produce =
    new Produce(StableHashProvider.hash(Seq(channel))(serializeC.toCodec),
                StableHashProvider.hash(channel, datum, persist))

  def fromHash(channelsHash: Blake2b256Hash, hash: Blake2b256Hash): Produce =
    new Produce(channelsHash, hash)

  implicit val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash]).as[Produce]
}

case class Consume private (channelsHash: Blake2b256Hash, hash: Blake2b256Hash) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String =
    s"Consume(channels: ${channelsHash.toString}, hash: ${hash.toString})"
}

object Consume {

  def unapply(arg: Consume): Option[(Blake2b256Hash, Blake2b256Hash)] =
    Some((arg.channelsHash, arg.hash))

  def create[C, P, K](channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]): Consume =
    new Consume(StableHashProvider.hash(channels)(serializeC.toCodec),
                StableHashProvider.hash(channels, patterns, continuation, persist))

  def fromHash(channelsHash: Blake2b256Hash, hash: Blake2b256Hash): Consume =
    new Consume(channelsHash, hash)

  implicit val codecConsume: Codec[Consume] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash]).as[Consume]
}
