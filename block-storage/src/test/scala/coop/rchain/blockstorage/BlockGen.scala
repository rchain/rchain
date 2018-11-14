package coop.rchain.blockstorage

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Header}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOfN

object BlockGen {
  // TODO: move to `shared` along with code in coop.rchain.rspace.test.ArbitraryInstances
  /**
  Credit: https://gist.github.com/etorreborre/d0616e704ed85d7276eb12b025df8ab0

   Distinct list of elements from a given arbitrary
    */
  def distinctListOf[T: Arbitrary] =
    distinctListOfGen(arbitrary[T])(_ == _)

  val blockHashGen: Gen[BlockHash] = for {
    byteArray <- listOfN(32, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  implicit val arbitraryHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)

  val blockElementGen: Gen[BlockMessage] =
    for {
      hash      <- arbitrary[BlockHash]
      validator <- arbitrary[Validator]
      version   <- arbitrary[Long]
      timestamp <- arbitrary[Long]
    } yield
      BlockMessage(blockHash = hash)
        .withHeader(Header().withVersion(version).withTimestamp(timestamp))
        .withSender(validator)

  val blockHashElementGen: Gen[(String, BlockMessage)] =
    blockElementGen.map(block => (block.blockHash.toStringUtf8, block))

  /**
  Distinct list of elements from a given generator
   with a maximum number of elements to discard
    */
  def distinctListOfGen[T](gen: Gen[T], maxDiscarded: Int = 1000)(
      comp: (T, T) => Boolean
  ): Gen[List[T]] = {
    val seen      = new scala.collection.mutable.ListBuffer[T]
    var discarded = 0

    Gen.sized { size =>
      if (size == seen.size) seen.toList
      else {
        while (seen.size <= size && discarded < maxDiscarded) gen.sample match {
          case Some(t) if !seen.exists(comp(t, _)) =>
            seen.+=:(t)
          case _ => discarded += 1
        }
        seen.toList
      }
    }
  }

  val blockHashElementsGen: Gen[List[(String, BlockMessage)]] =
    distinctListOfGen(blockHashElementGen)(_._1 == _._1)

  def blockElementsGen: Gen[List[BlockMessage]] =
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
