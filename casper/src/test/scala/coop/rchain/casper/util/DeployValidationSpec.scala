package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{DeployData, DeployDataProto}
import coop.rchain.crypto.signatures._
import org.scalatest.{FlatSpec, Matchers}

class DeployValidationSpec extends FlatSpec with Matchers {
  private val SHARD_ID = "root-shard"

  def createFromDeployDataProto(alg: SignaturesAlg) = {
    val deploy = DeployData(
      term = "Nil",
      timestamp = 111111,
      phloLimit = 1000000,
      phloPrice = 1,
      validAfterBlockNumber = 0L,
      shardId = SHARD_ID
    )

    val (privKey, _) = alg.newKeyPair
    val signed       = Signed(deploy, alg, privKey)
    val deployProto = DeployDataProto()
      .withSigAlgorithm(alg.name)
      .withSig(signed.sig)
      .withDeployer(ByteString.copyFrom(signed.pk.bytes))
      .withTerm(signed.data.term)
      .withTimestamp(signed.data.timestamp)
      .withPhloPrice(signed.data.phloPrice)
      .withPhloLimit(signed.data.phloLimit)
      .withValidAfterBlockNumber(signed.data.validAfterBlockNumber)
    DeployData.from(deployProto)
  }

  "Secp256k1" should "be valid signature algorithm to sign a deploy" in {
    val validAlgs = Seq(Secp256k1, Secp256k1Eth)

    validAlgs.foreach { d =>
      createFromDeployDataProto(d) shouldBe a[Right[_, _]]
    }
  }

  "Ed25519" should "be invalid signature algorithm to sign a deploy" in {
    val inValidAlgs = Seq(Ed25519)

    inValidAlgs.foreach { d =>
      createFromDeployDataProto(d) shouldBe a[Left[_, _]]
    }
  }

}
