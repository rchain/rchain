package coop.rchain.rspace.trace

import cats.implicits._
import coop.rchain.rspace.{Blake2b256Hash, Serialize}
import coop.rchain.shared.AttemptOps._
import scodec.bits.BitVector
import scodec.interop.cats._
import scodec.{Attempt, Codec}
import coop.rchain.rspace.internal._
import coop.rchain.scodec.codecs._
import scala.collection.immutable.Seq

/**
  * Broadly speaking, there are two kinds of events in RSpace,
  *
  *   1. [[IOEvent]]s, which are represented as [[Produce]] and [[Consume]]s
  *   2. [[COMM]] Events, which consist of a single [[Consume]] and one or more [[Produce]]s
  */
sealed trait Event
case class COMM(consume: Consume, produces: Seq[Produce]) extends Event

sealed trait IOEvent extends Event

class Produce private (val hash: Blake2b256Hash) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case produce: Produce => produce.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String = s"Produce(hash: ${hash.toString})"

  def unapply(arg: Produce): Option[Blake2b256Hash] = Some(arg.hash)
}

object Produce {

  def apply[C, A](channel: C, datum: A)(implicit
                                        serializeC: Serialize[C],
                                        serializeA: Serialize[A]): Produce = {
    implicit val codecC: Codec[C] = serializeC.toCodec
    implicit val codecA: Codec[A] = serializeA.toCodec

    val hash: Blake2b256Hash =
      List(Codec[C].encode(channel), Codec[A].encode(datum))
        .sequence[Attempt, BitVector]
        .map((vectors: List[BitVector]) => Blake2b256Hash.create(vectors.combineAll.toByteArray))
        .get

    new Produce(hash)
  }
}

class Consume private (val hash: Blake2b256Hash) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String = s"Consume(hash: ${hash.toString})"

  def unapply(arg: Consume): Option[Blake2b256Hash] = Some(arg.hash)
}

object Consume {

  def apply[C, P, K](channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]): Consume = {
    implicit val codecC: Codec[C] = serializeC.toCodec
    implicit val codecA: Codec[P] = serializeP.toCodec
    implicit val codecK: Codec[K] = serializeK.toCodec

    val hash: Blake2b256Hash =
      List(Codec[Seq[C]].encode(channels),
           Codec[Seq[P]].encode(patterns),
           Codec[K].encode(continuation))
        .sequence[Attempt, BitVector]
        .map((vectors: List[BitVector]) => Blake2b256Hash.create(vectors.combineAll.toByteArray))
        .get

    new Consume(hash)
  }
}
