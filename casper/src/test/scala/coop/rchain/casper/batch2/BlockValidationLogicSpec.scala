package coop.rchain.casper.batch2

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, DeployData, ProcessedDeploy}
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.casper.{BlockValidationLogic, ValidatorIdentity}
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.BlockVersion
import coop.rchain.models.blockImplicits.{arbBlockMessage, getRandomBlock}
import coop.rchain.models.syntax._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockValidationLogicSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  implicit private val aBlock: Arbitrary[BlockMessage] = arbBlockMessage

  "Block version validation" should "allow supported versions" in {
    forAll { (block: BlockMessage, version: Int) =>
      val blockWithVersion = block.copy(version = version)

      // Expected one of hard-coded block versions supported by this version of RNode software
      val expectedValid = BlockVersion.Supported.contains(version)
      // Actual validation
      val actualValid = BlockValidationLogic.version(blockWithVersion)

      actualValid shouldBe expectedValid
    }
  }

  "Block hash format validation" should "fail on invalid hash" in {
    forAll { (block: BlockMessage) =>
      val hash           = ProtoUtil.hashBlock(block)
      val blockValidHash = block.copy(blockHash = hash)

      // Test valid block hash
      val hashValid = BlockValidationLogic.blockHash(blockValidHash)

      hashValid shouldBe true

      val blockInValidHash = block.copy(blockHash = ByteString.copyFromUtf8("123"))

      // Test invalid block hash
      val hashInValid = BlockValidationLogic.blockHash(blockInValidHash)

      hashInValid shouldBe false
    }
  }

  "Field format validation" should "check that main fields are not empty" in {
    forAll { b: BlockMessage =>
      val expected = b.blockHash.nonEmpty &&
        b.sig.nonEmpty &&
        b.sigAlgorithm.nonEmpty &&
        b.shardId.nonEmpty &&
        b.postStateHash.nonEmpty

      BlockValidationLogic.formatOfFields(b) shouldBe expected
    }
  }

  // TODO: This test doesn't make sense because the signing is done by the algorithm in the ValidatorIdentity.
  //  The algorithm specified for the block is not used
  "Block signature validation" should "return false on unknown algorithms" in {
    val blockUnknownAlg = getRandomBlock().copy(sigAlgorithm = "unknownAlgorithm")
    val blockRsaAlg     = getRandomBlock().copy(sigAlgorithm = "RSA")

    BlockValidationLogic.blockSignature(blockUnknownAlg) shouldBe false
    BlockValidationLogic.blockSignature(blockRsaAlg) shouldBe false
  }

  it should "return true on valid secp256k1 signatures and false otherwise" in {
    val (privateKey, publicKey) = Secp256k1.newKeyPair
    val block                   = signedBlock(privateKey, publicKey)

    val emptyBSGen   = Gen.const(ByteString.EMPTY)
    val wrongBSGen   = Gen.listOf(arbByte.arbitrary).map(_.toArray.toByteString)
    val newPubKeyGen = Gen.resultOf[Unit, PublicKey](_ => Secp256k1.newKeyPair._2)
    val wrongSenderGen =
      Gen.oneOf(newPubKeyGen, emptyBSGen.map(PublicKey.apply), wrongBSGen.map(PublicKey.apply))
    val wrongSigGen = Gen.oneOf(emptyBSGen, wrongBSGen)

    forAll(arbBool.arbitrary, arbBool.arbitrary, wrongSenderGen, wrongSigGen) {
      (senderCorrect: Boolean, sigCorrect: Boolean, wrongPubKey: PublicKey, wrongSig: ByteString) =>
        val sender = if (senderCorrect) publicKey else wrongPubKey
        val sig    = if (sigCorrect) block.sig else wrongSig

        BlockValidationLogic.blockSignature(
          block.copy(sender = sender.bytes.toByteString, sig = sig)
        ) shouldBe (senderCorrect && sigCorrect)
    }
  }

  "Future deploy validation" should "work" in {
    val deploy = createDeploy(-1L)
    val block  = getRandomBlock(setDeploys = Seq(ProcessedDeploy.empty(deploy)).some)
    val status = BlockValidationLogic.futureTransaction(block)

    status shouldBe true
  }

  it should "not accept blocks with a deploy for a future block number" in {
    val deploy = createDeploy(Long.MaxValue)
    val block  = getRandomBlock(setDeploys = Seq(ProcessedDeploy.empty(deploy)).some)
    val status = BlockValidationLogic.futureTransaction(block)

    status shouldBe false
  }

  private def signedBlock(privateKey: PrivateKey, publicKey: PublicKey): BlockMessage =
    ValidatorIdentity(privateKey).signBlock(
      getRandomBlock(setValidator = publicKey.bytes.toByteString.some)
    )

  private def createDeploy(validAfterBlockNumber: Long): Signed[DeployData] = {
    val deploy = ConstructDeploy.sourceDeployNow("Nil")
    Signed(
      deploy.data.copy(validAfterBlockNumber = validAfterBlockNumber),
      Secp256k1,
      ConstructDeploy.defaultSec
    )
  }

  "Deploy expiration validation" should "work" in {
    val deploy = createDeploy(-1L)
    val block  = getRandomBlock(setDeploys = Seq(ProcessedDeploy.empty(deploy)).some)
    val status = BlockValidationLogic.transactionExpiration(block, expirationThreshold = 10)
    status shouldBe true
  }

  it should "not accept blocks with a deploy that is expired" in {
    val deploy = createDeploy(Long.MinValue)
    val block  = getRandomBlock(setDeploys = Seq(ProcessedDeploy.empty(deploy)).some)
    val status = BlockValidationLogic.transactionExpiration(block, expirationThreshold = 10)
    status shouldBe false
  }
}
