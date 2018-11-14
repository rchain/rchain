package coop.rchain.rspace

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.interop.cats._
import cats.implicits._

/**
  * Type class for serializing and deserializing values
  *
  * @tparam A The type to serialize and deserialize.
  */
trait Serialize[A] {

  def encode(a: A): ByteVector

  def decode(bytes: ByteVector): Either[Throwable, A]
}

object Serialize {

  def apply[A](implicit ev: Serialize[A]): Serialize[A] = ev

  implicit class RichSerialize[A](instance: Serialize[A]) {

    def toCodec: Codec[A] = new Codec[A] {

      private val sizeCodec = int64

      private val codec = variableSizeBytesLong(sizeCodec, bytes)

      def sizeBound: SizeBound = sizeCodec.sizeBound + bytes.sizeBound

      def encode(value: A): Attempt[BitVector] =
        codec.encode(instance.encode(value))

      def decode(bits: BitVector): Attempt[DecodeResult[A]] =
        codec
          .decode(bits)
          .flatMap { (value: DecodeResult[ByteVector]) =>
            Attempt.fromEither(
              value
                .traverse[Either[Throwable, ?], A]((vec: ByteVector) => instance.decode(vec))
                .leftMap((thw: Throwable) => Err(thw.getMessage))
            )
          }
    }
  }

}
