package coop.rchain.casper.batch2

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.GenesisBuilder.randomValidatorKeyPairs
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{BlockValidationLogic, ValidatorIdentity}
import coop.rchain.models.BlockVersion
import coop.rchain.models.blockImplicits.{arbBlockMessage, getRandomBlock}
import coop.rchain.models.syntax._
import org.scalacheck.Arbitrary
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

  "Field format validation" should "succeed on a valid block and fail on empty fields" in {
    val (privateKey, _) = randomValidatorKeyPairs.take(1).toList.head
    val genesis = ValidatorIdentity(privateKey).signBlock(
      getRandomBlock(setValidator = privateKey.bytes.toByteString.some)
    )

    BlockValidationLogic.formatOfFields(genesis) shouldBe true
    BlockValidationLogic.formatOfFields(genesis.copy(blockHash = ByteString.EMPTY)) shouldBe false
    BlockValidationLogic.formatOfFields(genesis.copy(sig = ByteString.EMPTY)) shouldBe false
    BlockValidationLogic.formatOfFields(genesis.copy(sigAlgorithm = "")) shouldBe false
    BlockValidationLogic.formatOfFields(genesis.copy(shardId = "")) shouldBe false
    BlockValidationLogic.formatOfFields(genesis.copy(postStateHash = ByteString.EMPTY)) shouldBe false
  }
}
