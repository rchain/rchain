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
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

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

}

object Produce {

  def unapply(arg: Produce): Option[Blake2b256Hash] = Some(arg.hash)

  val length: Int = 32

  def create[C, A](channel: C, datum: A, persist: Boolean)(implicit
                                                           serializeC: Serialize[C],
                                                           serializeA: Serialize[A]): Produce = {
    implicit val codecC: Codec[C] = serializeC.toCodec
    implicit val codecA: Codec[A] = serializeA.toCodec

    val hash: Blake2b256Hash =
      List(Codec[C].encode(channel), Codec[A].encode(datum), (ignore(7) ~> bool).encode(persist))
        .sequence[Attempt, BitVector]
        .map((vectors: List[BitVector]) => Blake2b256Hash.create(vectors.combineAll.toByteArray))
        .get

    new Produce(hash)
  }

  implicit val codecProduce: Codec[Produce] =
    Codec[Blake2b256Hash].as[Produce]
}

class Consume private (val hash: Blake2b256Hash) extends IOEvent {

  override def equals(obj: scala.Any): Boolean = obj match {
    case consume: Consume => consume.hash == hash
    case _                => false
  }

  override def hashCode(): Int = hash.hashCode()

  override def toString: String = s"Consume(hash: ${hash.toString})"
}

object Consume {

  val length: Int = 32

  def unapply(arg: Consume): Option[Blake2b256Hash] = Some(arg.hash)

  def create[C, P, K](channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
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
           Codec[K].encode(continuation),
           (ignore(7) ~> bool).encode(persist))
        .sequence[Attempt, BitVector]
        .map((vectors: List[BitVector]) => Blake2b256Hash.create(vectors.combineAll.toByteArray))
        .get

    new Consume(hash)
  }

  implicit val codecConsume: Codec[Consume] =
    Codec[Blake2b256Hash].as[Consume]
}
