package coop.rchain.casper.protocol

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class DeployDataSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit val ddArb: Arbitrary[DeployData] = Arbitrary(
    for {
      term                  <- arbString.arbitrary
      timestamp             <- arbLong.arbitrary
      phloPrice             <- arbLong.arbitrary
      phloLimit             <- arbLong.arbitrary
      validAfterBlockNumber <- arbLong.arbitrary
      shardId               <- arbString.arbitrary
    } yield DeployData(
      term,
      timestamp,
      phloPrice,
      phloLimit,
      validAfterBlockNumber,
      shardId
    )
  )

  property("Serialization roundtrip works fine") {
    forAll { (dd: DeployData) =>
      DeployData.serialize.decode(DeployData.serialize.encode(dd)) should be(Right(dd))
    }
  }

}
