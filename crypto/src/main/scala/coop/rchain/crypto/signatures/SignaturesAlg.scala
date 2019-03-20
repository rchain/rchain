package coop.rchain.crypto.signatures

trait SignaturesAlg {
  def verify(data: Array[Byte], signature: Array[Byte], pub: Array[Byte]): Boolean
  def sign(data: Array[Byte], sec: Array[Byte]): Array[Byte]
  def toPublic(sec: Array[Byte]): Array[Byte]
  def name: String
}

object SignaturesAlg {
  def apply(algorithm: String): Option[SignaturesAlg] =
    algorithm.toLowerCase match {
      case "ed25519"   => Some(Ed25519)
      case "secp256k1" => Some(Secp256k1)
      case _           => None
    }
}
