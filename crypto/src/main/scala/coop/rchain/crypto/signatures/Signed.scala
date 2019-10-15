package coop.rchain.crypto.signatures

import com.google.protobuf.ByteString
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.shared.Serialize

final case class Signed[A] private (data: A, sig: ByteString, sigAlgorithm: SignaturesAlg)

object Signed {
  def apply[A: Serialize](
      data: A,
      sigAlgorithm: SignaturesAlg,
      privateKey: PrivateKey
  ): Signed[A] = {
    val toSign = Serialize[A].encode(data).toArray
    val hash   = Blake2b256.hash(toSign)
    val sig    = sigAlgorithm.sign(hash, privateKey)

    Signed(data, ByteString.copyFrom(sig), sigAlgorithm)
  }
}
