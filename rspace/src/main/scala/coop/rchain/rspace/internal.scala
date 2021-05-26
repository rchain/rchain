package coop.rchain.rspace

import com.google.common.collect.{HashMultiset, Multiset}
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector

import scala.collection.SortedSet

object internal {

  /** helper class to package data and serialization (encoded by scodec) */
  final case class Encoded[D](item: D, byteVector: ByteVector)

  final case class Datum[A](a: A, persist: Boolean, source: Produce)

  object Datum {
    def create[A](channel: Channel, a: A, persist: Boolean)(
        implicit
        serializeA: Serialize[A]
    ): Datum[A] =
      Datum(a, persist, Produce(channel, a, persist))
  }

  final case class WaitingContinuation[P, K](
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      peeks: SortedSet[Int],
      source: Consume
  )

  object WaitingContinuation {
    def create[P, K](
        channels: Seq[Channel],
        patterns: Seq[P],
        continuation: K,
        persist: Boolean,
        peek: SortedSet[Int]
    )(
        implicit
        serializeP: Serialize[P],
        serializeK: Serialize[K]
    ): WaitingContinuation[P, K] =
      WaitingContinuation(
        patterns,
        continuation,
        persist,
        peek,
        Consume(channels, patterns, continuation, persist)
      )
  }

  final case class ConsumeCandidate[A](
      channel: Channel,
      datum: Datum[A],
      removedDatum: A,
      datumIndex: Int
  )

  final case class ProduceCandidate[P, A, K](
      channels: Seq[Channel],
      continuation: WaitingContinuation[P, K],
      continuationIndex: Int,
      dataCandidates: Seq[ConsumeCandidate[A]]
  )

  final case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  import scala.collection.concurrent.TrieMap

  type MultisetMultiMap[K, V] = TrieMap[K, Multiset[V]]

  object MultisetMultiMap {
    def empty[K, V]: MultisetMultiMap[K, V] = new TrieMap[K, Multiset[V]]()
  }

  implicit class RichMultisetMultiMap[K, V](private val value: MultisetMultiMap[K, V])
      extends AnyVal {

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
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

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
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
}
