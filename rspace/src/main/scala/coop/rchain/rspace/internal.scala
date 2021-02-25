package coop.rchain.rspace

import com.google.common.collect.{HashMultiset, Multiset}
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.scodec.codecs.seqOfN
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, bytes, int32, int64, uint8, variableSizeBytesLong}

import scala.collection.SortedSet

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object internal {

  /** helper class to package data and serialization (encoded by scodec) */
  final case class Encoded[D](item: D, byteVector: ByteVector)

  final case class Datum[A](a: A, persist: Boolean, source: Produce)

  object Datum {
    def create[C, A](channel: C, a: A, persist: Boolean)(
        implicit
        serializeC: Serialize[C],
        serializeA: Serialize[A]
    ): Datum[A] =
      Datum(a, persist, Produce.create(channel, a, persist))
  }

  final case class WaitingContinuation[P, K](
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int],
      source: Consume
  )

  object WaitingContinuation {
    def create[C, P, K](
        channels: Seq[C],
        patterns: Seq[P],
        continuation: K,
        persist: Boolean,
        peek: SortedSet[Int]
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
        peek,
        Consume.create(channels, patterns, continuation, persist)
      )
  }

  final case class ConsumeCandidate[C, A](
      channel: C,
      datum: Datum[A],
      removedDatum: A,
      datumIndex: Int
  )

  final case class ProduceCandidate[C, P, A, K](
      channels: Seq[C],
      continuation: WaitingContinuation[P, K],
      continuationIndex: Int,
      dataCandidates: Seq[ConsumeCandidate[C, A]]
  )

  final case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  implicit val codecByteVector: Codec[ByteVector] =
    variableSizeBytesLong(int64, bytes)

  implicit def codecSeq[A](implicit codecA: Codec[A]): Codec[Seq[A]] =
    seqOfN(int32, codecA)

  implicit def codecMap[K, V](implicit codecK: Codec[K], codecV: Codec[V]): Codec[Map[K, V]] =
    seqOfN(int32, codecK.pairedWith(codecV)).xmap(_.toMap, _.toSeq)

  implicit def codecDatum[A](implicit codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool :: Codec[Produce]).as[Datum[A]]

  def sortedSet[A](codecA: Codec[A])(implicit O: Ordering[A]): Codec[SortedSet[A]] =
    codecSeq[A](codecA).xmap[SortedSet[A]](s => SortedSet(s: _*), _.toSeq)

  implicit def codecWaitingContinuation[P, K](
      implicit
      codecP: Codec[P],
      codecK: Codec[K]
  ): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool :: sortedSet[Int](uint8) :: Codec[Consume])
      .as[WaitingContinuation[P, K]]

  import scala.collection.concurrent.TrieMap

  type MultisetMultiMap[K, V] = TrieMap[K, Multiset[V]]

  object MultisetMultiMap {
    def empty[K, V]: MultisetMultiMap[K, V] = new TrieMap[K, Multiset[V]]()
  }

  implicit class RichMultisetMultiMap[K, V](private val value: MultisetMultiMap[K, V])
      extends AnyVal {

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

  final case class Install[F[_], P, A, K](patterns: Seq[P], continuation: K)

  type Installs[F[_], C, P, A, K] = Map[Seq[C], Install[F, P, A, K]]

  import scodec.Attempt

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  implicit class RichAttempt[T](a: Attempt[T]) {
    def get: T =
      a match {
        case Attempt.Successful(res) => res
        case Attempt.Failure(err) =>
          throw new Exception("Data in RSpace is corrupted. " + err.messageWithContext)
      }
  }

  def toOrderedByteVectors[A](elements: Seq[A])(implicit serialize: Serialize[A]): Seq[ByteVector] =
    elements
      .map(e => serialize.encode(e))
      .sorted(util.ordByteVector)

  def toOrderedByteVectorsWithCodec[A](
      elements: Seq[A]
  )(implicit codecC: Codec[A]): Seq[ByteVector] =
    elements
      .map(e => codecC.encode(e).get.toByteVector)
      .sorted(util.ordByteVector)

  /** Datum with ByteVector representation */
  final case class RichDatum[A](decoded: Datum[A], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichDatum(_, r) => raw == r
      case _               => false
    }
  }

  object RichDatum {
    def create[A](datum: Datum[A])(implicit codecA: Codec[A]): RichDatum[A] =
      RichDatum(datum, Codec.encode[Datum[A]](datum).get.toByteVector)
  }

  /** Continuation with ByteVector representation */
  final case class RichKont[P, K](decoded: WaitingContinuation[P, K], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichKont(_, r) => raw == r
      case _              => false
    }
  }

  object RichKont {
    def create[P, K](
        wk: WaitingContinuation[P, K]
    )(implicit codecC: Codec[P], codecP: Codec[K]): RichKont[P, K] =
      RichKont(wk, Codec.encode[WaitingContinuation[P, K]](wk).get.toByteVector)
  }

  /** Join with ByteVector representation */
  final case class RichJoin[C](decoded: Seq[C], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichJoin(_, r) => raw == r
      case _              => false
    }
  }

  object RichJoin {
    def create[C](join: Seq[C])(implicit codecA: Codec[C]): RichJoin[C] =
      RichJoin(join, history.encodeJoin(join))
  }

}
