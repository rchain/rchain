package coop.rchain.rspace.scodecmodels

import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bool, bytes, int32, listOfN, variableSizeBytes}

private[rspace] case class PsKsBytes(patterns: List[ByteVector],
                                     kvalue: ByteVector,
                                     persist: Boolean)

private[rspace] case class AsBytes(avalue: ByteVector, persist: Boolean)

private[rspace] object rscodecs {
  lazy val byteVectorCodec: Codec[ByteVector] = variableSizeBytes(int32, bytes.xmap(x => x, x => x))

  lazy val bytesListCodec: Codec[List[ByteVector]] =
    listOfN(int32, byteVectorCodec).as[List[ByteVector]]

  lazy val asBytesCodec: Codec[AsBytes] = (byteVectorCodec :: bool).as[AsBytes]

  lazy val asBytesListCodec: Codec[List[AsBytes]] = listOfN(int32, asBytesCodec).as[List[AsBytes]]

  lazy val psKsBytesCodec: Codec[PsKsBytes] =
    (bytesListCodec :: byteVectorCodec :: bool).as[PsKsBytes]

  lazy val psKsBytesListCodec: Codec[List[PsKsBytes]] =
    listOfN(int32, psKsBytesCodec).as[List[PsKsBytes]]

  private[this] def fromAttempt[T](attempt: Attempt[DecodeResult[T]]): T =
    attempt.toEither match {
      case Right(res) => res.value
      case Left(err)  => throw new Exception(err.toString)
    }

  def toBitVector[T](value: T, codec: Codec[T]): BitVector =
    codec.encode(value).toEither match {
      case Right(res) => res
      case Left(err)  => throw new Exception(err.toString)
    }

  def fromBitVector[T](vector: BitVector, codec: Codec[T]): T =
    fromAttempt(codec.decode(vector))
}
