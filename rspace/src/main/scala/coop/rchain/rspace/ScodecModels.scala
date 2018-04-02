package coop.rchain.rspace.scodecmodels

import coop.rchain.rspace.Serialize
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bool, bytes, int32, listOfN, variableSizeBytes}

private[rspace] case class BytesList(values: List[ByteVector])

private[rspace] case class PsKsBytes(patterns: BytesList, kvalue: ByteVector, persist: Boolean)

private[rspace] case class PsKsBytesList(values: List[PsKsBytes])

private[rspace] case class AsBytes(avalue: ByteVector, persist: Boolean)

private[rspace] case class AsBytesList(values: List[AsBytes])

private[rspace] object rscodecs {
  lazy val byteVectorCodec: Codec[ByteVector] = variableSizeBytes(int32, bytes.xmap(x => x, x => x))

  lazy val bytesListCodec: Codec[BytesList] = listOfN(int32, byteVectorCodec).as[BytesList]

  lazy val asBytesCodec: Codec[AsBytes] = (byteVectorCodec :: bool).as[AsBytes]

  lazy val asBytesListCodec: Codec[AsBytesList] = listOfN(int32, asBytesCodec).as[AsBytesList]

  lazy val psKsBytesCodec: Codec[PsKsBytes] =
    (bytesListCodec :: byteVectorCodec :: bool).as[PsKsBytes]

  lazy val psKsBytesListCodec: Codec[PsKsBytesList] =
    listOfN(int32, psKsBytesCodec).as[PsKsBytesList]

  def fromAttempt[T](attempt: Attempt[DecodeResult[T]]): T =
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

  def fromScodecArray[T](bytes: Array[Byte], codec: Codec[T]): T =
    fromAttempt(codec.decode(BitVector(bytes)))

  def mkScodecInstance[T](codec: Codec[T]): Serialize[T] = new Serialize[T] {
    override def encode(a: T): Array[Byte] = toBitVector(a, codec).toByteArray

    override def decode(bytes: Array[Byte]): Either[Throwable, T] =
      codec.decode(BitVector(bytes)).toEither match {
        case Right(res) => Right(res.value)
        case Left(err)  => throw new Exception(err.toString)
      }
  }
}
