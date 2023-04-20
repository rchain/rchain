package coop.rchain.scodec.codecs

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}

final class SeqCodec[A](codec: Codec[A], limit: Option[Int] = None) extends Codec[Seq[A]] {

  def sizeBound: SizeBound = limit match {
    case None      => SizeBound.unknown
    case Some(lim) => codec.sizeBound * lim.toLong
  }

  def encode(list: Seq[A]): Attempt[BitVector] =
    Encoder.encodeSeq(codec)(Seq(list: _*))

  def decode(buffer: BitVector): Attempt[DecodeResult[Seq[A]]] =
    Decoder.decodeCollect[Seq, A](codec, limit)(buffer)

  override def toString = s"seq($codec)"
}
