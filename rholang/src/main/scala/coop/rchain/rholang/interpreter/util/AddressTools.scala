package coop.rchain.rholang.interpreter.util

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rholang.interpreter.util.codec.Base58

final case class Address(prefix: Array[Byte], keyHash: Array[Byte], checksum: Array[Byte]) {

  def toBase58: String = {
    val payload = prefix ++ keyHash
    val address = payload ++ checksum
    Base58.encode(address)
  }
}

class AddressTools(prefix: Array[Byte], keyLength: Int, checksumLength: Int) {
  private def computeChecksum(toCheck: Array[Byte]): Array[Byte] =
    Blake2b256.hash(toCheck).take(checksumLength)

  /**
    * Creates an Address given a public key.
    *
    * @param pk the public key from which the address is derived
    * @return None if the key length is invalid or Some if the address was created successfully
    */
  def fromPublicKey(pk: PublicKey): Option[Address] =
    if (keyLength == pk.bytes.length || 32 == pk.bytes.length) {
      val keyHash = Blake2b256.hash(pk.bytes)
      val payload = prefix ++ keyHash

      Some(Address(prefix, keyHash, computeChecksum(payload)))
    } else {
      None
    }

  def isValid(address: String): Boolean = parse(address).isRight

  private val checksumStart = prefix.length + Blake2b256.hashLength
  private val addressLength = prefix.length + Blake2b256.hashLength + checksumLength

  def parse(address: String): Either[String, Address] = {
    def validateLength(bytes: Array[Byte]) =
      if (bytes.length == addressLength)
        Right(())
      else
        Left("Invalid address length")

    def validateChecksum(bytes: Array[Byte]) = {
      val (payload, checksum) = bytes.splitAt(checksumStart)
      val computedChecksum    = computeChecksum(payload)

      if (computedChecksum.deep == checksum.deep)
        Right((payload, checksum))
      else
        Left("Invalid checksum")
    }

    def parseKeyHash(payload: Array[Byte]) = {
      val (actualPrefix, keyHash) = payload.splitAt(prefix.length)

      if (actualPrefix.deep == prefix.deep)
        Right(keyHash)
      else Left("Invalid prefix")
    }

    for {
      decodedAddress <- Base58
                         .decode(address)
                         .toRight("Invalid Base58 encoding")
      _                   <- validateLength(decodedAddress)
      payloadAndChecksum  <- validateChecksum(decodedAddress)
      (payload, checksum) = payloadAndChecksum
      keyHash             <- parseKeyHash(payload)
    } yield Address(prefix, keyHash, checksum)
  }
}
