package coop.rchain.crypto.signatures

import com.google.protobuf.ByteString
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.shared.Serialize

final case class Signed[A] private (data: A, pk : PublicKey, sig: ByteString, sigAlgorithm: SignaturesAlg)

object Signed {
  def apply[A: Serialize](
      data: A,
      sigAlgorithm: SignaturesAlg,
      sk: PrivateKey
  ): Signed[A] = {
    val toSign = Serialize[A].encode(data).toArray
    val hash   = Blake2b256.hash(toSign)
    val sig    = sigAlgorithm.sign(hash, sk)

    Signed(data, sigAlgorithm.toPublic(sk), ByteString.copyFrom(sig), sigAlgorithm)
  }

  def fromSignedData[A:Serialize](
      data : A,
      pk : PublicKey,
      sig : ByteString,
      sigAlgorithm : SignaturesAlg
  ) : Option[Signed[A]] = {
    val hash   = Blake2b256.hash(Serialize[A].encode(data).toArray)

    if (sigAlgorithm.verify(hash,sig.toByteArray, pk))
      Some(new Signed(data, pk, sig, sigAlgorithm))
    else
      None
  }
}
