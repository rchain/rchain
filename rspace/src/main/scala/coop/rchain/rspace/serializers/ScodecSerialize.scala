package coop.rchain.rspace.serializers

import coop.rchain.rspace.hashing.{Blake2b256Hash, ChannelHash}
import coop.rchain.rspace.history.PointerBlock.length
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.rspace.{util, Channel}
import coop.rchain.scodec.codecs.seqOfN
import coop.rchain.shared.Serialize
import coop.rchain.shared.Serialize._
import scodec.bits.ByteVector
import scodec.codecs.{bool, discriminated, int32, provide, uint, uint2, uint8, vectorOfN}
import scodec.{Attempt, Codec}

import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap

/**
  * This file represents RSpace serializers based on scodec library.
  *
  * With [[Serialize]] interface RSpace accepts parametrized serializers
  * for the main types (channel, pattern, attributes, continuation).
  *
  * In the current implementation RSpace internally also uses concrete scodec
  * serializers for additional types and collections.
  */
object ScodecSerialize {

  /*
   * Datum serializer
   */

  def encodeDatums[A](datums: Seq[Datum[A]])(implicit sa: Serialize[A]): ByteVector = {
    val codec = serializeToCodecDatumMemo(sa)

    encodeSortedSeq[Datum[A]](datums, codec)
  }

  def decodeDatums[A](bytes: ByteVector)(implicit sa: Serialize[A]): Seq[Datum[A]] =
    decodeDatumsProj[A, Datum[A]](bytes)((d, _) => d)

  def decodeDatumsProj[A, R](
      bytes: ByteVector
  )(proj: (Datum[A], ByteVector) => R)(implicit sa: Serialize[A]): Seq[R] = {
    val codec = serializeToCodecDatumMemo(sa)

    decodeSeqProj(bytes, codec)(proj)
  }

  /*
   * Continuation serializer
   */

  def encodeContinuations[P, K](konts: Seq[WaitingContinuation[P, K]])(
      implicit
      sp: Serialize[P],
      sk: Serialize[K]
  ): ByteVector = {
    val codec = serializeToCodecContinuationMemo(sp, sk)

    encodeSortedSeq[WaitingContinuation[P, K]](konts, codec)
  }

  def decodeContinuations[P, K](bytes: ByteVector)(
      implicit
      sp: Serialize[P],
      sk: Serialize[K]
  ): Seq[WaitingContinuation[P, K]] =
    decodeContinuationsProj[P, K, WaitingContinuation[P, K]](bytes)((d, _) => d)

  def decodeContinuationsProj[P, K, R](bytes: ByteVector)(
      proj: (WaitingContinuation[P, K], ByteVector) => R
  )(
      implicit
      sp: Serialize[P],
      sk: Serialize[K]
  ): Seq[R] = {
    val codec = serializeToCodecContinuationMemo(sp, sk)

    decodeSeqProj(bytes, codec)(proj)
  }

  /*
   * Joins serializer
   */

  def encodeJoins(joins: Seq[Seq[Channel]]): ByteVector = {
    val joinsEncoded =
      joins.map(cs => codecSeqByteVector.encode(cs.map(_.hash.bytes)).getUnsafe.toByteVector)

    codecSeqByteVector.encode(joinsEncoded).getUnsafe.toByteVector
  }

  def decodeJoins(bytes: ByteVector): Seq[Seq[Channel]] =
    decodeJoinsProj[Seq[Channel]](bytes)((d, _) => d)

  def decodeJoinsProj[R](
      bytes: ByteVector
  )(proj: (Seq[Channel], ByteVector) => R): Seq[R] =
    codecSeqByteVector
      .decode(bytes.bits)
      .getUnsafe
      .value
      .map { bv =>
        val joins = codecSeqByteVector
          .decode(bv.bits)
          .getUnsafe
          .value
          .map(bv => Blake2b256Hash.fromByteArray(bv.toArray))
          .map(Channel)
        proj(joins, bv)
      }

  def toOrderedByteVectors[A](elements: Seq[A])(implicit serialize: Serialize[A]): Seq[ByteVector] =
    elements
      .map(serialize.encode)
      .sorted(util.ordByteVector)

  /*
   * Serializers for [[Datum]] and [[WaitingContinuation]]
   */

  private def codecDatum[A](codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool :: codecProduce).as[Datum[A]]

  private def codecWaitingContinuation[P, K](
      codecP: Codec[P],
      codecK: Codec[K]
  ): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool :: sortedSet(uint8) :: codecConsume)
      .as[WaitingContinuation[P, K]]

//  private def codecChannel: Codec[Channel] =
//    (codecByteVector).as[Channel]

  /*
   * Serializers for RSpace event log
   */

  implicit def codecEvent: Codec[Event] =
    discriminated[Event]
      .by(uint2)
      .subcaseP(tag = 0) {
        case comm: COMM => comm
      }(codecCOMM)
      .subcaseP(tag = 1) {
        case produce: Produce => produce
      }(codecProduce)
      .subcaseP(tag = 2) {
        case consume: Consume => consume
      }(codecConsume)

  implicit def codecLog: Codec[Seq[Event]] = codecSeq[Event](codecEvent)

  private val codecProduce: Codec[Produce] =
    (Codec[Blake2b256Hash] :: Codec[Blake2b256Hash] :: bool).as[Produce]

  private val codecConsume: Codec[Consume] =
    (codecSeq[Blake2b256Hash] :: Codec[Blake2b256Hash] :: bool).as[Consume]

  private val codecCOMM: Codec[COMM] =
    (codecConsume :: codecSeq(codecProduce) :: sortedSet(uint8) :: codecMap(codecProduce, int32))
      .as[COMM]

  val codecSkip: Codec[Skip] = (codecByteVector :: codecTrieValuePointer).as[Skip]

  val memoizingSkipCodec: Codec[Skip] =
    Codec.apply((s: Skip) => Attempt.successful(s.encoded), codecSkip.decode)

  val memoizingPointerBlockCodec: Codec[PointerBlock] =
    Codec.apply(
      (s: PointerBlock) => Attempt.successful(s.encoded),
      codecPointerBlock.decode
    )

  /*
   * scodec for History types
   */

  val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      codecTriePointer
    ).as[PointerBlock]

  val codecTrie: Codec[Trie] =
    discriminated[Trie]
      .by(uint2)
      .subcaseP(0) {
        case e: EmptyTrie.type => e
      }(provide(EmptyTrie))
      .subcaseP(1) {
        case s: Skip => s
      }(memoizingSkipCodec)
      .subcaseP(2) {
        case pb: PointerBlock => pb
      }(memoizingPointerBlockCodec)

  implicit def codecTriePointer: Codec[TriePointer] =
    discriminated[TriePointer]
      .by(uint2)
      .subcaseP(0) {
        case p: EmptyPointer.type => p
      }(provide(EmptyPointer))
      .subcaseP(1) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[LeafPointer])
      .subcaseP(2) {
        case p: SkipPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[SkipPointer])
      .subcaseP(3) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[NodePointer])

  implicit def codecTrieValuePointer: Codec[ValuePointer] =
    discriminated[ValuePointer]
      .by(uint(1))
      .subcaseP(0) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[LeafPointer])
      .subcaseP(1) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecWithBytesStringBlake2b256Hash.as[NodePointer])

  /*
   * Converters from Serialize to scodec
   */

//  private def serializeToCodec[A](sa: Serialize[A]): Codec[A] =
//    sa.toSizeHeadCodec

  private def serializeToCodecDatum[A](sa: Serialize[A]): Codec[Datum[A]] = {
    val codecA = sa.toSizeHeadCodec
    codecDatum(codecA)
  }

  private def serializeToCodecContinuation[P, K](
      sp: Serialize[P],
      sk: Serialize[K]
  ): Codec[WaitingContinuation[P, K]] = {
    val codecP = sp.toSizeHeadCodec
    val codecK = sk.toSizeHeadCodec
    codecWaitingContinuation(codecP, codecK)
  }

  /*
   * Simple memoization for generated scodec from Serialize interface
   */

  private val memoSt = TrieMap[Any, Any]()

  private def memoize[A, B](prefix: String, f: A => B): A => B = { key =>
    memoSt.getOrElseUpdate((prefix, key), f(key)).asInstanceOf[B]
  }

//  private def serializeToCodecMemo[A]: Serialize[A] => Codec[A] =
//    memoize("Codec", serializeToCodec)

  private def serializeToCodecDatumMemo[A]: Serialize[A] => Codec[Datum[A]] =
    memoize("Datum", serializeToCodecDatum)

  private def serializeToCodecContinuationMemo[P, K](
      sp: Serialize[P],
      sk: Serialize[K]
  ): Codec[WaitingContinuation[P, K]] =
    memoize("Cont", (serializeToCodecContinuation[P, K] _).tupled)(sp, sk)

  /*
   * scodec serializers (collection, map, set)
   */

  val codecSeqByteVector: Codec[Seq[ByteVector]] = codecSeq(Serialize.codecByteVector)

  private def codecSeq[A](implicit codecA: Codec[A]): Codec[Seq[A]] =
    seqOfN(int32, codecA)

  private def codecMap[K, V](implicit codecK: Codec[K], codecV: Codec[V]): Codec[Map[K, V]] =
    seqOfN(int32, codecK.pairedWith(codecV)).xmap(_.toMap, _.toSeq)

  private def sortedSet[A](codecA: Codec[A])(implicit O: Ordering[A]): Codec[SortedSet[A]] =
    codecSeq[A](codecA).xmap[SortedSet[A]](s => SortedSet(s: _*), _.toSeq)

  /*
   * scodec sequence encoder/decoder
   */

  private def encodeSortedSeq[D](data: Seq[D], codec: Codec[D]): ByteVector =
    codecSeqByteVector
      .encode(data.map(codec.encode(_).getUnsafe.toByteVector).toVector.sorted(util.ordByteVector))
      .getUnsafe
      .toByteVector

//  private def decodeSeq[D](data: ByteVector, codec: Codec[D]): Seq[D] =
//    decodeSeqProj(data, codec)((d, _) => d)

  private def decodeSeqProj[D, R](data: ByteVector, codec: Codec[D])(
      proj: (D, ByteVector) => R
  ): Seq[R] =
    codecSeqByteVector
      .decode(data.bits)
      .getUnsafe
      .value
      .map(bv => proj(codec.decode(bv.bits).getUnsafe.value, bv))

  /*
   * RSpace values with attached raw binary encoded data
   */

  /** Datum with ByteVector representation */
  final case class DatumB[A](decoded: Datum[A], raw: ByteVector)
      extends WrapWithBinary[Datum[A]](raw)

  /** Continuation with ByteVector representation */
  final case class WaitingContinuationB[P, K](decoded: WaitingContinuation[P, K], raw: ByteVector)
      extends WrapWithBinary[WaitingContinuation[P, K]](raw)

  /** Joins with ByteVector representation */
  final case class JoinsB(decoded: Seq[Channel], raw: ByteVector)
      extends WrapWithBinary[Seq[Channel]](raw)

  /**
    * Equality for Datum, WaitingContinuation and Joins defined with binary equality.
    */
  sealed abstract class WrapWithBinary[A](raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case DatumB(_, r)               => raw == r
      case WaitingContinuationB(_, r) => raw == r
      case JoinsB(_, r)               => raw == r
      case _                          => false
    }
  }

  /**
    * scodec extension to unsafe get value
    *
    * TODO: Very similar extension is defined in shared [[coop.rchain.shared.AttemptOps.RichAttempt]]
    */
  import scodec.Attempt

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  implicit class RichAttempt[T](a: Attempt[T]) {
    def getUnsafe: T =
      a match {
        case Attempt.Successful(res) => res
        case Attempt.Failure(err) =>
          throw new Exception("Data in RSpace is corrupted. " + err.messageWithContext)
      }
  }
}
