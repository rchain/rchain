package coop.rchain.rspace.trace

import coop.rchain.rspace.StableHashProvider
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import coop.rchain.rspace.{Blake2b256Hash, Serialize}
import coop.rchain.rspace.internal.codecSeq
import coop.rchain.rspace.trace.Produce.logger

import scala.collection.immutable.Seq
import scodec.Codec
import scodec.codecs._
import scodec.codecs.implicits._

/**
  * Broadly speaking, there are two kinds of events in RSpace,
  *
  *   1. [[IOEvent]]s, which are represented as [[Produce]] and [[Consume]]s
  *   2. [[COMM]] Events, which consist of a single [[Consume]] and one or more [[Produce]]s
  */
sealed trait Event

object Event extends StrictLogging {

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

case class Produce(
    channelsHash: Blake2b256Hash,
    hash: Blake2b256Hash,
    channel: String = "",
    datum: String = "",
    persist: String = ""
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String =
    s"Produce(channel: $channel, datum: $datum, persist: $persist)"

}

object Produce extends StrictLogging {

  def unapply(arg: Produce): Option[(Blake2b256Hash, Blake2b256Hash, String, String, String)] =
    Some((arg.channelsHash, arg.hash, arg.channel, arg.datum, arg.persist))

  def create[C, A](channel: C, datum: A, persist: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ): Produce = {
    logger.debug("channel: {}", channel.toString)
    logger.debug("datum: {}", datum.toString)

    val hashch = StableHashProvider.hash(Seq(channel))(serializeC.toCodec)
    val hashd  = StableHashProvider.hash(channel, datum, persist)

    logger.debug("channel hash: {}", hashch)
    logger.debug("datum hash: {}", hashd)

    new Produce(
      hashch,
      hashd,
      channel.toString,
      datum.toString,
      persist.toString
    )
  }

  def fromHash(
      channelsHash: Blake2b256Hash,
      hash: Blake2b256Hash,
      channel: String,
      datum: String,
      persist: String
  ): Produce =
    new Produce(channelsHash, hash, channel, datum, persist)

  implicit val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: Codec[String] :: Codec[String] :: Codec[
      String
    ]).as[Produce]
}

case class Consume(
    channelsHash: Blake2b256Hash,
    hash: Blake2b256Hash,
    channels: String = "",
    patterns: String = "",
    continuation: String = "",
    persist: String = ""
) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String =
    s"Consume(channels: $channels, patterns: $patterns, continuation: $continuation, persist: $persist)"
}

object Consume extends StrictLogging {

  def unapply(
      arg: Consume
  ): Option[(Blake2b256Hash, Blake2b256Hash, String, String, String, String)] =
    Some((arg.channelsHash, arg.hash, arg.channels, arg.patterns, arg.continuation, arg.persist))

  def create[C, P, K](channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ): Consume = {
    logger.debug("channels: {}", channels.toString)
    logger.debug("patterns: {}", patterns.toString)
    logger.debug("cont: {}", continuation.toString)
    val hashch  = StableHashProvider.hash(channels)(serializeC.toCodec)
    val hashpat = StableHashProvider.hash(channels, patterns, continuation, persist)

    logger.debug("channel hash: {}", hashch)
    logger.debug("pattern hash: {}", hashpat)

    new Consume(
      hashch,
      hashpat,
      channels.toString,
      patterns.toString,
      continuation.toString,
      persist.toString
    )
  }

  def fromHash(
      channelsHash: Blake2b256Hash,
      hash: Blake2b256Hash,
      channels: String,
      patterns: String,
      continuation: String,
      persist: String
  ): Consume =
    new Consume(channelsHash, hash, channels, patterns, continuation, persist)

  implicit val codecConsume: Codec[Consume] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: Codec[String] :: Codec[String] :: Codec[
      String
    ] :: Codec[String]).as[Consume]
}
