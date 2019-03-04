package coop.rchain.rholang.interpreter.util

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.rholang.interpreter.util.codec.Base58

final case class Address(val prefix: Array[Byte], val keyHash: Array[Byte])

class AddressTools(prefix: Array[Byte], keyLength: Int, checksumLength: Int) {
  private def computeChecksum(toCheck: Array[Byte]): Array[Byte] =
    Blake2b256.hash(toCheck).take(checksumLength)

  /**
    * Creates an Address given a public key.
    *
    * @param pk the public key from which the address is derived
    * @return None if the key length is invalid or Some if the address was created successfully
    */
  def fromPublicKey(pk: PublicKey): Option[String] =
    if (keyLength == pk.bytes.length) {
      val keyHash = Blake2b256.hash(pk.bytes)
      val payload = prefix ++ keyHash
      val address = payload ++ computeChecksum(payload)

      Some(Base58.encode(address))
    } else None

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
        Right(payload)
      else
        Left("Invalid checksum")
    }

    def parsePayload(payload: Array[Byte]) = {
      val (actualPrefix, keyHash) = payload.splitAt(prefix.length)

      if (actualPrefix.deep == prefix.deep) Right(Address(actualPrefix, keyHash))
      else Left("Invalid prefix")
    }

    for {
      decodedAddress <- Base58.decode(address)
                              .toRight("Invalid Base58 encoding")
      _              <- validateLength(decodedAddress)
      payload        <- validateChecksum(decodedAddress)
      address        <- parsePayload(payload)
    } yield address
  }
}
