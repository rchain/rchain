package coop.rchain.casper.util

import coop.rchain.models.blockImplicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProtoUtilTest extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "dependenciesHashesOf" should "return hashes of all justifications and parents of a block" in {
    forAll(blockElementGen()) { blockElement =>
      val result               = ProtoUtil.dependenciesHashesOf(blockElement)
      val justificationsHashes = blockElement.justifications
      val parentsHashes        = blockElement.header.parentsHashList
      result should contain allElementsOf (justificationsHashes)
      result should contain allElementsOf (parentsHashes)
      result should contain theSameElementsAs ((justificationsHashes ++ parentsHashes).toSet)
    }
  }
}
