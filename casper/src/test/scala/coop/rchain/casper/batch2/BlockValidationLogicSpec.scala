package coop.rchain.casper.batch2

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockValidationLogic
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.models.BlockVersion
import coop.rchain.models.blockImplicits.arbBlockMessage
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
}
