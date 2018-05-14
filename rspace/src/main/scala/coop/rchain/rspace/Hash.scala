package coop.rchain.rspace

import coop.rchain.crypto.hash.Blake2b256

class Hash private (val bytes: Array[Byte])

object Hash {

  def create(bytes: Array[Byte]): Hash =
    new Hash(Blake2b256.hash(bytes))

  implicit object serializeHash extends Serialize[Hash] {

    def encode(a: Hash): Array[Byte] = a.bytes

    def decode(bytes: Array[Byte]): Either[Throwable, Hash] = Right(new Hash(bytes))
  }
}
