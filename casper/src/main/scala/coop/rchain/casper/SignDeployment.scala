package coop.rchain.casper

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures._
import com.google.protobuf.ByteString

object SignDeployment {

  private def fill(
      deployData: DeployData
  )(user: PublicKey, sig: Array[Byte], sigAlgorithm: String): DeployData =
    deployData
      .withUser(ByteString.copyFrom(user.bytes))
      .withSig(ByteString.copyFrom(sig))
      .withSigAlgorithm(sigAlgorithm)

  private def clear(deployData: DeployData): DeployData =
    fill(deployData)(PublicKey(Array.empty[Byte]), Array.empty[Byte], "")

  def apply(key: PrivateKey, deployData: DeployData): DeployData = {
    val toSign    = clear(deployData).toByteString.toByteArray
    val hash      = Blake2b256.hash(toSign)
    val signature = Ed25519.sign(hash, key.bytes)

    fill(deployData)(PublicKey(Ed25519.toPublic(key.bytes)), signature, "Ed25519".toLowerCase)
  }

}
