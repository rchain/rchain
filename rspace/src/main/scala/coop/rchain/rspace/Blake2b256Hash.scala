package coop.rchain.rspace

import coop.rchain.crypto.hash.Blake2b256

/**
  * Represents a Blake2b256 Hash
  *
  * The default constructor is private to prevent construction means other than [[create]]
  *
  * TODO: Restrict access
  */
class Blake2b256Hash private (val bytes: Array[Byte])

object Blake2b256Hash {

  /**
    * Constructs a [[Blake2b256Hash]]
    *
    * @param bytes The bytes to hash
    * @return The hash
    */
  def create(bytes: Array[Byte]): Blake2b256Hash =
    new Blake2b256Hash(Blake2b256.hash(bytes))

  implicit object serializeHash extends Serialize[Blake2b256Hash] {

    def encode(a: Blake2b256Hash): Array[Byte] = a.bytes

    def decode(bytes: Array[Byte]): Either[Throwable, Blake2b256Hash] =
      Right(new Blake2b256Hash(bytes))
  }
}
