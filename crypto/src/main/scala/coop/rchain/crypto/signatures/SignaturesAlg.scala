package coop.rchain.crypto.signatures
import coop.rchain.crypto.{PrivateKey, PublicKey}

import java.util.Locale

trait SignaturesAlg {
  def verify(data: Array[Byte], signature: Array[Byte], pub: Array[Byte]): Boolean
  def sign(data: Array[Byte], sec: Array[Byte]): Array[Byte]
  def toPublic(sec: PrivateKey): PublicKey
  def newKeyPair: (PrivateKey, PublicKey)
  def name: String

  def verify(data: Array[Byte], signature: Array[Byte], pub: PublicKey): Boolean =
    verify(data, signature, pub.bytes)
  def sign(data: Array[Byte], sec: PrivateKey): Array[Byte] = sign(data, sec.bytes)

  val sigLength: Int
}

object SignaturesAlg {
  def apply(algorithm: String): Option[SignaturesAlg] =
    algorithm.toLowerCase(Locale.getDefault) match {
      // ed25519 signature algorithm is disabled
      // TODO: quick way to prevent use of ed25519 to sign deploys
      // https://rchain.atlassian.net/browse/RCHAIN-3560
      // case Ed25519.name => Some(Ed25519)

      case Secp256k1.name    => Some(Secp256k1)
      case Secp256k1Eth.name => Some(Secp256k1Eth)
      case _                 => None
    }
}
