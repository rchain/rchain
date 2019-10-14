package coop.rchain.casper

import cats._, cats.data._, cats.implicits._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures._
import com.google.protobuf.ByteString
import scala.util.Try

object SignDeployment {

  private def fill(
      deployData: DeployData
  )(
      deployer: PublicKey,
      sig: Array[Byte],
      sigAlgorithm: String,
      validAfterBlockNumber: Long
  ): DeployData =
    deployData.copy(
      deployer = ByteString.copyFrom(deployer.bytes),
      sig = ByteString.copyFrom(sig),
      sigAlgorithm = sigAlgorithm,
      validAfterBlockNumber = validAfterBlockNumber
    )

  private def clear(deployData: DeployData): DeployData =
    fill(deployData)(PublicKey(Array.empty[Byte]), Array.empty[Byte], "", 0L)

  def sign(sec: PrivateKey, deployData: DeployData, alg: SignaturesAlg = Secp256k1): DeployData = {
    val toSign    = DeployData.toProto(clear(deployData)).toByteString.toByteArray
    val hash      = Blake2b256.hash(toSign)
    val signature = alg.sign(hash, sec)

    fill(deployData)(alg.toPublic(sec), signature, alg.name, deployData.validAfterBlockNumber)
  }

  def verify(deployData: DeployData): Option[Boolean] =
    SignaturesAlg(deployData.sigAlgorithm) >>= { alg =>
      val toVerify = DeployData.toProto(clear(deployData)).toByteString.toByteArray
      val hash     = Blake2b256.hash(toVerify)
      Try(alg.verify(hash, deployData.sig.toByteArray, PublicKey(deployData.deployer.toByteArray))).toOption
    }

}
