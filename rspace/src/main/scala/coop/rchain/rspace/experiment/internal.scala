package coop.rchain.rspace
package experiment

import com.google.common.collect.{HashMultiset, Multiset}
import coop.rchain.scodec.codecs.seqOfN
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, bytes, int32, int64, uint8, variableSizeBytesLong}

import scala.collection.SortedSet

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object internal {

  final case class Datum[A](a: A, persist: Boolean)

  object Datum {
    def create[C, A](channel: C, a: A, persist: Boolean)(
        ): Datum[A] =
      Datum(a, persist)
  }

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int]
  )

  object WaitingContinuation {
    def create[C, P, K](
        channels: Seq[C],
        patterns: Seq[P],
        continuation: K,
        persist: Boolean,
        peek: SortedSet[Int]
    )(
        ): WaitingContinuation[P, K] =
      WaitingContinuation(patterns, continuation, persist, peek)
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
    (codecA :: bool).as[Datum[A]]

  def sortedSet[A](codecA: Codec[A])(implicit O: Ordering[A]): Codec[SortedSet[A]] =
    codecSeq[A](codecA).xmap[SortedSet[A]](s => SortedSet(s: _*), _.toSeq)

  implicit def codecWaitingContinuation[P, K](
      implicit
      codecP: Codec[P],
      codecK: Codec[K]
  ): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool :: sortedSet[Int](uint8))
      .as[WaitingContinuation[P, K]]

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

  final case class Install[F[_], P, A, R, K](
      patterns: Seq[P],
      continuation: K,
      _match: Match[F, P, A, R]
  )

  type Installs[F[_], C, P, A, R, K] = Map[Seq[C], Install[F, P, A, R, K]]

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

}
