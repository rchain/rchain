package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.shared.GeneratorUtils._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOfN

object blockImplicits {

  val blockHashGen: Gen[ByteString] = for {
    byteArray <- listOfN(32, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  implicit val arbitraryHash: Arbitrary[ByteString] = Arbitrary(blockHashGen)

  val justificationGen: Gen[Justification] = for {
    latestBlockHash <- arbitrary[ByteString]
  } yield Justification().withLatestBlockHash(latestBlockHash)

  implicit val arbitraryJustification: Arbitrary[Justification] = Arbitrary(justificationGen)

  val blockElementGen: Gen[BlockMessage] =
    for {
      hash            <- arbitrary[ByteString]
      validator       <- arbitrary[ByteString]
      version         <- arbitrary[Long]
      timestamp       <- arbitrary[Long]
      parentsHashList <- arbitrary[Seq[ByteString]]
      justifications  <- arbitrary[Seq[Justification]]
    } yield
      BlockMessage(blockHash = hash)
        .withHeader(
          Header()
            .withParentsHashList(parentsHashList)
            .withVersion(version)
            .withTimestamp(timestamp)
        )
        .withSender(validator)

  val blockElementsGen: Gen[List[BlockMessage]] =
    Gen.listOf(blockElementGen)

  val blockBatchesGen: Gen[List[List[BlockMessage]]] =
    Gen.listOf(blockElementsGen)

  def blockElementsWithParentsGen: Gen[List[BlockMessage]] =
    Gen.sized { size =>
      (0 until size).foldLeft(Gen.listOfN(0, blockElementGen)) {
        case (gen, _) =>
          for {
            blocks       <- gen
            b            <- blockElementGen
            parents      <- Gen.someOf(blocks)
            parentHashes = parents.map(_.blockHash)
            newBlock     = b.withHeader(b.header.get.withParentsHashList(parentHashes))
          } yield newBlock :: blocks
      }
    }

  def blockWithNewHashesGen(blockElements: List[BlockMessage]): Gen[List[BlockMessage]] =
    Gen.listOfN(blockElements.size, blockHashGen).map { blockHashes =>
      blockElements.zip(blockHashes).map {
        case (b, hash) => b.withBlockHash(hash)
      }
    }
}
