package coop.rchain.casper

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures._
import com.google.protobuf.ByteString

object SignDeployment {

  private def fill(
      deployData: DeployData
  )(deployer: PublicKey, sig: Array[Byte], sigAlgorithm: String): DeployData =
    deployData.copy(
      deployer = ByteString.copyFrom(deployer.bytes),
      sig = ByteString.copyFrom(sig),
      sigAlgorithm = sigAlgorithm
    )

  private def clear(deployData: DeployData): DeployData =
    fill(deployData)(PublicKey(Array.empty[Byte]), Array.empty[Byte], "")

  def sign(sec: PrivateKey, deployData: DeployData, alg: SignaturesAlg = Secp256k1): DeployData = {
    val toSign    = DeployData.toProto(clear(deployData)).toByteString.toByteArray
    val hash      = Blake2b256.hash(toSign)
    val signature = alg.sign(hash, sec)

    fill(deployData)(alg.toPublic(sec), signature, alg.name)
  }

  def verify(deployData: DeployData): Option[Boolean] =
    SignaturesAlg(deployData.sigAlgorithm).map { alg =>
      val toVerify = DeployData.toProto(clear(deployData)).toByteString.toByteArray
      val hash     = Blake2b256.hash(toVerify)
      alg.verify(hash, deployData.sig.toByteArray, PublicKey(deployData.deployer.toByteArray))
    }

}
