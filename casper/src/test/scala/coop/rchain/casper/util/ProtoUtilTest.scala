package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.models.blockImplicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.listOfN
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ProtoUtilTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "dependenciesHashesOf" should "return hashes of all justifications and parents of a block" in {
    forAll(blockElementGen) { blockElement =>
      val result = ProtoUtil.dependenciesHashesOf(blockElement)
      val justificationsHashes = blockElement.justifications.map(
        _.latestBlockHash
      )
      val parentsHashes = blockElement.header.toSeq.flatMap(_.parentsHashList)
      result should contain allElementsOf (justificationsHashes)
      result should contain allElementsOf (parentsHashes)
      result should contain theSameElementsAs ((justificationsHashes ++ parentsHashes).toSet)
    }
  }
}
