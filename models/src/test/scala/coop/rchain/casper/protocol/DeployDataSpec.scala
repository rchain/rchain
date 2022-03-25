package coop.rchain.casper.protocol

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DeployDataSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  private val SHARD_ID = "root-shard"

  implicit val ddArb: Arbitrary[DeployData] = Arbitrary(
    for {
      term                  <- arbString.arbitrary
      timestamp             <- arbLong.arbitrary
      phloPrice             <- arbLong.arbitrary
      phloLimit             <- arbLong.arbitrary
      validAfterBlockNumber <- arbLong.arbitrary
    } yield DeployData(
      term,
      timestamp,
      phloPrice,
      phloLimit,
      validAfterBlockNumber,
      shardId = SHARD_ID
    )
  )

  property("Serialization roundtrip works fine") {
    forAll { (dd: DeployData) =>
      DeployData.serialize.decode(DeployData.serialize.encode(dd)) should be(Right(dd))
    }
  }

}
