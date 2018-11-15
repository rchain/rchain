package coop.rchain.rspace

import com.google.common.collect.{HashMultiset, Multiset}
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.scodec.codecs.seqOfN
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, bytes, int32, int64, variableSizeBytesLong}

import scala.collection.immutable.Seq
import scala.collection.mutable

object internal {

  final case class Datum[A](a: A, persist: Boolean, source: Produce)

  object Datum {
    def create[C, A](channel: C, a: A, persist: Boolean, sequenceNumber: Int = 0)(
        implicit
        serializeC: Serialize[C],
        serializeA: Serialize[A]
    ): Datum[A] =
      Datum(a, persist, Produce.create(channel, a, persist, sequenceNumber))
  }

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      source: Consume
  )

  object WaitingContinuation {
    def create[C, P, K](
        channels: Seq[C],
        patterns: Seq[P],
        continuation: K,
        persist: Boolean,
        sequenceNumber: Int = 0
    )(
        implicit
        serializeC: Serialize[C],
        serializeP: Serialize[P],
        serializeK: Serialize[K]
    ): WaitingContinuation[P, K] =
      WaitingContinuation(
        patterns,
        continuation,
        persist,
        Consume.create(channels, patterns, continuation, persist, sequenceNumber)
      )
  }

  final case class ProduceCandidate[C, P, A, K](
      channels: Seq[C],
      continuation: WaitingContinuation[P, K],
      continuationIndex: Int,
      dataCandidates: Seq[DataCandidate[C, A]]
  )

  final case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  /** [[GNAT]] is not a `Tuple3`
    */
  final case class GNAT[C, P, A, K](
      channels: Seq[C],
      data: Seq[Datum[A]],
      wks: Seq[WaitingContinuation[P, K]]
  )

  sealed trait Operation extends Product with Serializable
  case object Insert     extends Operation
  case object Delete     extends Operation

  final case class TrieUpdate[C, P, A, K](
      count: Long,
      operation: Operation,
      channelsHash: Blake2b256Hash,
      gnat: GNAT[C, P, A, K]
  )

  implicit val codecByteVector: Codec[ByteVector] =
    variableSizeBytesLong(int64, bytes)

  implicit def codecSeq[A](implicit codecA: Codec[A]): Codec[Seq[A]] =
    seqOfN(int32, codecA)

  implicit def codecDatum[A](implicit codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool :: Codec[Produce]).as[Datum[A]]

  implicit def codecWaitingContinuation[P, K](
      implicit
      codecP: Codec[P],
      codecK: Codec[K]
  ): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool :: Codec[Consume]).as[WaitingContinuation[P, K]]

  implicit def codecGNAT[C, P, A, K](
      implicit
      codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): Codec[GNAT[C, P, A, K]] =
    (codecSeq(codecC) ::
      codecSeq(codecDatum(codecA)) ::
      codecSeq(codecWaitingContinuation(codecP, codecK))).as[GNAT[C, P, A, K]]

  import scala.collection.concurrent.TrieMap
  type MultisetMultiMap[K, V] = TrieMap[K, Multiset[V]]

  object MultisetMultiMap {
    def empty[K, V]: MultisetMultiMap[K, V] = new TrieMap[K, Multiset[V]]()
  }

  implicit class RichMultisetMultiMap[K, V](val value: MultisetMultiMap[K, V]) extends AnyVal {

    def addBinding(k: K, v: V): MultisetMultiMap[K, V] =
      value.get(k) match {
        case Some(current) =>
          current.add(v)
          value
        case None =>
          val ms = HashMultiset.create[V]()
          ms.add(v)
          value.putIfAbsent(k, ms)
          value
      }

    def removeBinding(k: K, v: V): MultisetMultiMap[K, V] =
      value.get(k) match {
        case Some(current) =>
          current.remove(v)
          if (current.isEmpty) {
            value.remove(k, current)
          }
          value
        case None =>
          value
      }
  }

  case class Install[P, E, A, R, K](patterns: Seq[P], continuation: K, _match: Match[P, E, A, R])

  type Installs[C, P, E, A, R, K] = Map[Seq[C], Install[P, E, A, R, K]]

  import scodec.{Attempt, Err}

  implicit class RichAttempt[T](a: Attempt[T]) {
    def get: T =
      a match {
        case Attempt.Successful(res) => res
        case Attempt.Failure(err) =>
          throw new Exception("Data in RSpace is corrupted. " + err.messageWithContext)
      }
  }
}
