package coop.rchain.rspace.serializers

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.trace.{COMM, Consume, Event, Produce}
import coop.rchain.rspace.{util, Blake2b256Hash}
import coop.rchain.scodec.codecs.seqOfN
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, discriminated, int32, uint2, uint8}

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

  def encodeData[A](datums: Seq[Datum[A]])(implicit sa: Serialize[A]): ByteVector = {
    val codec = serializeToCodecDatumMemo(sa)

    encodeSortedSeq[Datum[A]](datums, codec)
  }

  def decodeData[A](bytes: ByteVector)(implicit sa: Serialize[A]): Seq[Datum[A]] =
    decodeDataProj[A, Datum[A]](bytes)((d, _) => d)

  def decodeDataProj[A, R](
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

  def encodeJoins[C](joins: Seq[Seq[C]])(implicit sc: Serialize[C]): ByteVector = {
    val codec = serializeToCodecMemo(sc)

    codecSeqByteVector
      .encode(joins.map(encodeSortedSeq(_, codec)).toVector.sorted(util.ordByteVector))
      .get
      .toByteVector
  }

  def decodeJoins[C](bytes: ByteVector)(implicit sc: Serialize[C]): Seq[Seq[C]] =
    decodeJoinsProj[C, Seq[C]](bytes)((d, _) => d)

  def decodeJoinsProj[C, R](
      bytes: ByteVector
  )(proj: (Seq[C], ByteVector) => R)(implicit sc: Serialize[C]): Seq[R] = {
    val codec = serializeToCodecMemo(sc)

    codecSeqByteVector
      .decode(bytes.bits)
      .get
      .value
      .map(bv => proj(decodeSeq(bv, codec), bv))
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

  /*
   * Converters from Serialize to scodec
   */

  private def serializeToCodec[A](sa: Serialize[A]): Codec[A] =
    sa.toSizeHeadCodec

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

  private def serializeToCodecMemo[A]: Serialize[A] => Codec[A] =
    memoize("Codec", serializeToCodec)

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
      .encode(data.map(codec.encode(_).get.toByteVector).toVector.sorted(util.ordByteVector))
      .get
      .toByteVector

  private def decodeSeq[D](data: ByteVector, codec: Codec[D]): Seq[D] =
    decodeSeqProj(data, codec)((d, _) => d)

  private def decodeSeqProj[D, R](data: ByteVector, codec: Codec[D])(
      proj: (D, ByteVector) => R
  ): Seq[R] =
    codecSeqByteVector
      .decode(data.bits)
      .get
      .value
      .map(bv => proj(codec.decode(bv.bits).get.value, bv))

  /*
   * RSpace values with raw binary encoded data
   */

  /** Datum with ByteVector representation */
  final case class RichDatum[A](decoded: Datum[A], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichDatum(_, r) => raw == r
      case _               => false
    }
  }

  /** Continuation with ByteVector representation */
  final case class RichKont[P, K](decoded: WaitingContinuation[P, K], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichKont(_, r) => raw == r
      case _              => false
    }
  }

  /** Join with ByteVector representation */
  final case class RichJoin[C](decoded: Seq[C], raw: ByteVector) {
    override def hashCode(): Int = raw.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case RichJoin(_, r) => raw == r
      case _              => false
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
    def get: T =
      a match {
        case Attempt.Successful(res) => res
        case Attempt.Failure(err) =>
          throw new Exception("Data in RSpace is corrupted. " + err.messageWithContext)
      }
  }
}
