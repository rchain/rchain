package coop.rchain.rspace

import coop.rchain.rspace.internal._
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, uint8}

import scala.collection.concurrent.TrieMap

package object history {

  /**
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

  /**
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

  /**
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

  /**
    * Serializers for [[Datum]] and [[WaitingContinuation]]
    */
  private def codecDatum[A](codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool :: Produce.codecProduce).as[Datum[A]]

  private def codecWaitingContinuation[P, K](
      codecP: Codec[P],
      codecK: Codec[K]
  ): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool :: sortedSet[Int](uint8) :: Consume.codecConsume)
      .as[WaitingContinuation[P, K]]

  /**
    * Converters from Serialize to scodecs
    */
  private def serializeToCodec[A](sa: Serialize[A]): Codec[A] = {
    sa.toSizeHeadCodec
  }

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

  /**
    * Simple memoization for generated scodecs from Serialize interface
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

  /**
    * Sequence scodecs
    */
  private val codecSeqByteVector: Codec[Seq[ByteVector]] = codecSeq(codecByteVector)

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
}
