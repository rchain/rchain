package coop.rchain.rholang.interpreter.util

import java.util.Arrays
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.models.GPrivate
import coop.rchain.rholang.interpreter.util.codec.Base58
import coop.rchain.shared.Base16

final case class Address(prefix: Array[Byte], keyHash: Array[Byte], checksum: Array[Byte]) {

  def toBase58: String = {
    val payload = prefix ++ keyHash
    val address = payload ++ checksum
    Base58.encode(address)
  }

  override def equals(o: Any): Boolean = o match {
    case other @ Address(_, _, _) =>
      prefix.sameElements(other.prefix) &&
        keyHash.sameElements(other.keyHash) &&
        checksum.sameElements(other.checksum)
    case _ => false
  }

  override def hashCode(): Int =
    Arrays.hashCode(
      Array(
        Arrays.hashCode(prefix),
        Arrays.hashCode(keyHash),
        Arrays.hashCode(checksum)
      )
    )
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
    if (keyLength == pk.bytes.length) {
      val ethAddress = Base16.encode(Keccak256.hash(pk.bytes.drop(1))).takeRight(40)
      fromEthAddress(ethAddress)
    } else None

  def fromEthAddress(ethAddress: String): Option[Address] = {
    val ethAddressWithoutPrefix = if (ethAddress.startsWith("0x")) {
      ethAddress.drop(2)
    } else {
      ethAddress
    }
    if (ethAddressWithoutPrefix.length == ETH_ADDRESS_LENGTH) {
      val keyHash = Keccak256.hash(Base16.unsafeDecode(ethAddressWithoutPrefix))
      val payload = prefix ++ keyHash
      Some(Address(prefix, keyHash, computeChecksum(payload)))
    } else None
  }

  def fromUnforgeable(gprivate: GPrivate): Address = {
    val keyHash = Keccak256.hash(gprivate.toByteArray)
    val payload = prefix ++ keyHash
    Address(prefix, keyHash, computeChecksum(payload))
  }

  def isValid(address: String): Boolean = parse(address).isRight

  private val checksumStart      = prefix.length + Blake2b256.hashLength
  private val addressLength      = prefix.length + Blake2b256.hashLength + checksumLength
  private val ETH_ADDRESS_LENGTH = 40

  def parse(address: String): Either[String, Address] = {
    def validateLength(bytes: Array[Byte]) =
      if (bytes.length == addressLength)
        Right(())
      else
        Left("Invalid address length")

    def validateChecksum(bytes: Array[Byte]) = {
      val (payload, checksum) = bytes.splitAt(checksumStart)
      val computedChecksum    = computeChecksum(payload)

      if (computedChecksum.toIndexedSeq == checksum.toIndexedSeq)
        Right((payload, checksum))
      else
        Left("Invalid checksum")
    }

    def parseKeyHash(payload: Array[Byte]) = {
      val (actualPrefix, keyHash) = payload.splitAt(prefix.length)

      if (actualPrefix.toIndexedSeq == prefix.toIndexedSeq)
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
