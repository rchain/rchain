package coop.rchain.rspace

import coop.rchain.rspace.internal._
import scodec.Codec
import scodec.bits.ByteVector

package object history {

  /** datum codecs */
  def encodeData[A](datums: Seq[Datum[A]])(implicit codec: Codec[Datum[A]]): ByteVector =
    encodeSorted[Datum[A]](datums)

  def decodeData[A](bytes: ByteVector)(implicit codec: Codec[Datum[A]]): Seq[Datum[A]] =
    decodeSorted[Datum[A]](bytes)

  def encodeDataRich[A](datums: Seq[Datum[A]])(
      implicit codec: Codec[Datum[A]]
  ): (ByteVector, Seq[Encoded[Datum[A]]]) = encodeSortedRich[Datum[A]](datums)

  def decodeDataRich[A](
      bytes: ByteVector
  )(implicit codec: Codec[Datum[A]]): Seq[Encoded[Datum[A]]] =
    decodeSortedRich[Datum[A]](bytes)

  /** continuations codecs */
  def encodeContinuations[P, K](konts: Seq[WaitingContinuation[P, K]])(
      implicit codec: Codec[WaitingContinuation[P, K]]
  ): ByteVector =
    encodeSorted[WaitingContinuation[P, K]](konts)

  def decodeContinuations[P, K](bytes: ByteVector)(
      implicit codec: Codec[WaitingContinuation[P, K]]
  ): Seq[WaitingContinuation[P, K]] =
    decodeSorted[WaitingContinuation[P, K]](bytes)

  def encodeContinuationsRich[P, K](konts: Seq[WaitingContinuation[P, K]])(
      implicit codec: Codec[WaitingContinuation[P, K]]
  ): (ByteVector, Seq[Encoded[WaitingContinuation[P, K]]]) =
    encodeSortedRich[WaitingContinuation[P, K]](konts)

  def decodeContinuationsRich[P, K](bytes: ByteVector)(
      implicit codec: Codec[WaitingContinuation[P, K]]
  ): Seq[Encoded[WaitingContinuation[P, K]]] =
    decodeSortedRich[WaitingContinuation[P, K]](bytes)

  /** joins codecs */
  def encodeJoins[C](joins: Seq[Seq[C]])(implicit codec: Codec[C]): ByteVector =
    codecSeqByteVector
      .encode(
        joins.par
          .map(
            channels => encodeSorted(channels)
          )
          .toVector
          .sorted(util.ordByteVector)
      )
      .get
      .toByteVector

  def decodeJoins[C](bytes: ByteVector)(implicit codec: Codec[C]): Seq[Seq[C]] =
    codecSeqByteVector
      .decode(bytes.bits)
      .get
      .value
      .par
      .map(
        bv => codecSeqByteVector.decode(bv.bits).get.value.map(v => codec.decode(v.bits).get.value)
      )
      .toVector

  def encodeJoin[C](join: Seq[C])(implicit codec: Codec[C]): ByteVector = encodeSorted(join)

  def encodeJoinsRich[C](
      joins: Seq[Seq[C]]
  )(implicit codec: Codec[C]): (ByteVector, Seq[Encoded[Seq[C]]]) = {
    val encodedJoins = joins.par
      .map(join => Encoded(join, encodeSorted(join)))
      .toVector
      .sortBy(v => v.byteVector)(util.ordByteVector)

    val result = codecSeqByteVector.encode(encodedJoins.map(_.byteVector)).get.toByteVector

    (result, encodedJoins)
  }

  def decodeJoinsRich[C](bytes: ByteVector)(implicit codec: Codec[C]): Seq[Encoded[Seq[C]]] =
    codecSeqByteVector
      .decode(bytes.bits)
      .get
      .value
      .par
      .map(
        bv =>
          Encoded[Seq[C]](
            codecSeqByteVector.decode(bv.bits).get.value.map(v => codec.decode(v.bits).get.value),
            bv
          )
      )
      .toVector

  /** private */
  private val codecSeqByteVector: Codec[Seq[ByteVector]] = codecSeq(codecByteVector)

  private def encodeSorted[D](data: Seq[D])(implicit codec: Codec[D]): ByteVector =
    codecSeqByteVector
      .encode(
        data.par
          .map(d => Codec.encode[D](d).get.toByteVector)
          .toVector
          .sorted(util.ordByteVector)
      )
      .get
      .toByteVector

  private def decodeSorted[D](data: ByteVector)(implicit codec: Codec[D]): Seq[D] =
    codecSeqByteVector
      .decode(data.bits)
      .get
      .value
      .par
      .map(bv => codec.decode(bv.bits).get.value)
      .toVector

  private def encodeSortedRich[D](
      items: Seq[D]
  )(implicit codec: Codec[D]): (ByteVector, Seq[Encoded[D]]) = {
    val itemsEncoded = items.par
      .map(i => Encoded(i, Codec.encode[D](i).get.toByteVector))
      .toVector
      .sortBy(v => v.byteVector)(util.ordByteVector)

    val byteVectors = itemsEncoded.map(_.byteVector)

    val result = codecSeqByteVector
      .encode(byteVectors)
      .get
      .toByteVector

    (result, itemsEncoded)
  }

  private def decodeSortedRich[D](
      data: ByteVector
  )(implicit codec: Codec[D]): Seq[Encoded[D]] =
    codecSeqByteVector
      .decode(data.bits)
      .get
      .value
      .par
      .map(bv => Encoded(codec.decode(bv.bits).get.value, bv))
      .toVector
}
