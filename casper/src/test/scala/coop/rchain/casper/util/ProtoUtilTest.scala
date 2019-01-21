package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.listOfN
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ProtoUtilTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val blockHashGen: Gen[BlockHash] = for {
    byteArray <- listOfN(32, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  implicit val arbitraryHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)

  val justificationGen: Gen[Justification] = for {
    latestBlockHash <- arbitrary[BlockHash]
  } yield (Justification().withLatestBlockHash(latestBlockHash))

  implicit val arbitraryJustification: Arbitrary[Justification] = Arbitrary(justificationGen)

  val blockElementGen: Gen[BlockMessage] =
    for {
      hash            <- arbitrary[BlockHash]
      version         <- arbitrary[Long]
      timestamp       <- arbitrary[Long]
      parentsHashList <- arbitrary[Seq[BlockHash]]
      justifications  <- arbitrary[Seq[Justification]]
    } yield
      BlockMessage(blockHash = hash)
        .withJustifications(justifications)
        .withHeader(
          Header()
            .withParentsHashList(parentsHashList)
            .withVersion(version)
            .withTimestamp(timestamp)
        )

  implicit val arbitraryBlock: Arbitrary[BlockMessage] = Arbitrary(blockElementGen)

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
