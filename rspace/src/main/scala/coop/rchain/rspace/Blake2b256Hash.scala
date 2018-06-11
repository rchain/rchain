package coop.rchain.rspace

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

/**
  * Represents a Blake2b256 Hash
  *
  * The default constructor is private to prevent construction means other than [[create]]
  */
class Blake2b256Hash private (val bytes: ByteVector) {

  require(bytes.length == Blake2b256Hash.length)

  override def toString: String = s"Blake2b256Hash(bytes: ${bytes.toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case b: Blake2b256Hash => b.bytes === bytes
    case _                 => false
  }

  override def hashCode(): Int =
    bytes.hashCode
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

  def fromHex(string: String): Blake2b256Hash =
    new Blake2b256Hash(ByteVector(Base16.decode(string)))

  implicit val codecBlake2b256Hash: Codec[Blake2b256Hash] =
    fixedSizeBytes(length.toLong, bytes).as[Blake2b256Hash]
}
