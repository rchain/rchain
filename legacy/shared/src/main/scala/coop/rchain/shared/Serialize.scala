package coop.rchain.shared

import cats.syntax.all._
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Attempt, Codec, Err}

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

  val codecByteVector: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)

  private def serializeToCodec[A](ser: Serialize[A]): Codec[A] =
    codecByteVector.exmap[A](
      bv => {
        val ea = ser.decode(bv).leftMap(ex => Err(ex.getMessage))
        Attempt.fromEither(ea)
      },
      a => {
        val bv = ser.encode(a)
        Attempt.successful(bv)
      }
    )

  implicit class RichSerialize[A](private val instance: Serialize[A]) extends AnyVal {
    def toSizeHeadCodec: Codec[A] = serializeToCodec(instance)
  }
}
