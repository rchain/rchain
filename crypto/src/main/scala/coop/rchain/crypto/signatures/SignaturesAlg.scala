package coop.rchain.crypto.signatures
import coop.rchain.crypto.{PrivateKey, PublicKey}

trait SignaturesAlg {
  def verify(data: Array[Byte], signature: Array[Byte], pub: Array[Byte]): Boolean
  def sign(data: Array[Byte], sec: Array[Byte]): Array[Byte]
  def toPublic(sec: PrivateKey): PublicKey
  def name: String

  def verify(data: Array[Byte], signature: Array[Byte], pub: PublicKey): Boolean =
    verify(data, signature, pub.bytes)
  def sign(data: Array[Byte], sec: PrivateKey): Array[Byte] = sign(data, sec.bytes)
}

object SignaturesAlg {
  def apply(algorithm: String): Option[SignaturesAlg] =
    algorithm.toLowerCase match {
      case "ed25519"   => Some(Ed25519)
      case "secp256k1" => Some(Secp256k1)
      case _           => None
    }
}
