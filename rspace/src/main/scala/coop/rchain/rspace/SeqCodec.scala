package coop.rchain.rspace

import scodec.{Attempt, Codec, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector

import scala.collection.immutable.Seq

private[rspace] final class SeqCodec[A](codec: Codec[A], limit: Option[Int] = None)
    extends Codec[Seq[A]] {

  def sizeBound = limit match {
    case None      => SizeBound.unknown
    case Some(lim) => codec.sizeBound * lim.toLong
  }

  def encode(list: Seq[A]) = Encoder.encodeSeq(codec)(list)

  def decode(buffer: BitVector) = Decoder.decodeCollect[Seq, A](codec, limit)(buffer)

  override def toString = s"seq($codec)"
}

private[rspace] object SeqCodec {

  /**
		* Codec that encodes/decodes a `Seq[A]` of `N` elements using a `Codec[A]`.
		*/
  def seqOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[Seq[A]] =
    countCodec
      .flatZip(count => new SeqCodec(valueCodec, Some(count)))
      .narrow[Seq[A]](
        {
          case (cnt, xs) =>
            if (xs.size == cnt) Attempt.successful(xs)
            else
              Attempt.failure(Err(
                s"Insufficient number of elements: decoded ${xs.size} but should have decoded $cnt"))
        },
        xs => (xs.size, xs)
      )
      .withToString(s"seqOfN($countCodec, $valueCodec)")
}
