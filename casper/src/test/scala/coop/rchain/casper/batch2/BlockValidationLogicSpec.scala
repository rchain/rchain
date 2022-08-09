package coop.rchain.casper.batch2

import coop.rchain.casper.BlockValidationLogic
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockVersion
import coop.rchain.models.blockImplicits.arbBlockMessage
import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockValidationLogicSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Block version validation" should "allow supported versions" in {
    implicit val aBlock: Arbitrary[BlockMessage] = arbBlockMessage

    forAll { (block: BlockMessage, version: Int) =>
      val blockWithVersion = block.copy(version = version)

      // Expected one of hard-coded block versions supported by this version of RNode software
      val expectedValid = BlockVersion.Supported.contains(version)
      // Actual validation
      val actualValid = BlockValidationLogic.version(blockWithVersion)

      actualValid shouldBe expectedValid
    }
  }
}
