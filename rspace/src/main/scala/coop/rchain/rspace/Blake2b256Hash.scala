package coop.rchain.rspace

import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rspace.internal.codecSeq
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

/**
  * Represents a Blake2b256 Hash
  *
  * The default constructor is private to prevent construction means other than [[Blake2b256Hash$.create(bytes:*]] or [[Blake2b256Hash$.create(byteVectors:*]]
  */
class Blake2b256Hash private (val bytes: ByteVector) {

  require(
    bytes.length == Blake2b256Hash.length,
    s"Expected ${Blake2b256Hash.length} but got ${bytes.length}"
  )

  override def equals(obj: scala.Any): Boolean = obj match {
    case b: Blake2b256Hash => b.bytes === bytes
    case _                 => false
  }

  override def hashCode(): Int =
    bytes.hashCode

  def toByteString: ByteString =
    ByteString.copyFrom(bytes.toArray)

  override def toString: String = s"Blake(${bytes.toHex})"
}

object Blake2b256Hash {

  val length: Int = 32

  /**
    * Constructs a [[Blake2b256Hash]]
    *
    * @param bytes The bytes to hash
    * @return The hash
    */
  def create(bytes: Array[Byte]): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(Blake2b256.hash(bytes)))

  /**
    * Constructs a [[Blake2b256Hash]]
    *
    * @param byteVectors sequence of byte vectors,
    * that will be hashed as a single concatenated
    * bytes string
    * @return The hash
    */
  def create(byteVectors: Seq[ByteVector]): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(Blake2b256.hash(byteVectors: _*)))

  def create(byteVector: ByteVector): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(Blake2b256.hash(byteVector)))

  def fromHex(string: String): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(Base16.unsafeDecode(string)))

  def fromByteArray(bytes: Array[Byte]): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(bytes))

  def fromByteString(byteString: ByteString): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(byteString.toByteArray))

  implicit val codecWithBytesStringBlake2b256Hash: Codec[Blake2b256Hash] =
    fixedSizeBytes(length.toLong, bytes).as[Blake2b256Hash]

  implicit val codecSeqBlake2b256Hash: Codec[Seq[Blake2b256Hash]] = codecSeq(
    codecWithBytesStringBlake2b256Hash
  )

  implicit val ordering: Ordering[Blake2b256Hash] =
    (x: Blake2b256Hash, y: Blake2b256Hash) => {
      x.bytes.toHex.compare(y.bytes.toHex)
    }
}
