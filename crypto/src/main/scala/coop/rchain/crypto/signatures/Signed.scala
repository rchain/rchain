package coop.rchain.crypto.signatures

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.shared.Serialize

final case class Signed[A] private (
    data: A,
    pk: PublicKey,
    sig: ByteString,
    sigAlgorithm: SignaturesAlg
)

object Signed {
  def apply[A: Serialize](
      data: A,
      sigAlgorithm: SignaturesAlg,
      sk: PrivateKey
  ): Signed[A] = {
    val serializedData = Serialize[A].encode(data).toArray
    val hash           = signatureHash(sigAlgorithm.name, serializedData)
    val sig            = sigAlgorithm.sign(hash, sk)

    Signed(data, sigAlgorithm.toPublic(sk), ByteString.copyFrom(sig), sigAlgorithm)
  }

  def fromSignedData[A: Serialize](
      data: A,
      pk: PublicKey,
      sig: ByteString,
      sigAlgorithm: SignaturesAlg
  ): Option[Signed[A]] = {
    val serializedData = Serialize[A].encode(data).toArray
    val hash           = signatureHash(sigAlgorithm.name, serializedData)

    if (sigAlgorithm.verify(hash, sig.toByteArray, pk))
      Some(new Signed(data, pk, sig, sigAlgorithm))
    else
      None
  }

  def signatureHash(sigAlgName: String, serializedData: Array[Byte]) =
    sigAlgName match {
      case Secp256k1Eth.name =>
        Keccak256.hash(ethPrefix(serializedData.length) ++ serializedData)
      case _ =>
        Blake2b256.hash(serializedData)
    }

  private[this] def ethPrefix(msgLength: Int) =
    s"\u0019Ethereum Signed Message:\n${msgLength}"
      .getBytes(StandardCharsets.UTF_8)
}
