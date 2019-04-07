package coop.rchain.casper.util
import coop.rchain.crypto.signatures.SignaturesAlg

object SignatureAlgorithms {
  type SignatureAlgorithm = (Array[Byte], Array[Byte]) => Array[Byte]
  @SuppressWarnings(Array("org.wartremover.warts.Throw")) // TODO remove throw
  def lookup(algorithm: String): SignatureAlgorithm =
    SignaturesAlg(algorithm)
      .fold(throw new Exception(s"Unknown signature algorithm $algorithm"))(_.sign)
}
