package coop.rchain.casper.util
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}

object SignatureAlgorithms {
  type SignatureAlgorithm = (Array[Byte], Array[Byte]) => Array[Byte]
  def lookup(algorithm: String): SignatureAlgorithm =
    algorithm match {
      case "ed25519"   => Ed25519.sign
      case "secp256k1" => Secp256k1.sign
      case _           => throw new Exception(s"Unknown signature algorithm $algorithm")
    }
}
