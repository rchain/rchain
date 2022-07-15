package coop.rchain.casper.rholang

import coop.rchain.crypto.PublicKey
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.scalatestcontrib.convertToAnyShouldWrapper
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.Checkers
import scodec.bits.ByteVector
import scodec.bits.ByteVector.fromHex

class BlockRandomSeedSpec extends AnyFlatSpec with Checkers {

  // Arbitrary[BlockRandomSeed]
  val blockRandomSeedGen = for {
    shardId     <- Gen.stringOfN(6, Gen.alphaChar)
    blockNumber <- Gen.choose(0L, 1000000L)
    senderList  <- Gen.listOfN(120, Gen.hexChar)
    hash        <- Gen.listOfN(Blake2b256Hash.length, Gen.hexChar)
  } yield {
    val sender       = PublicKey(senderList.map(_.toByte).toArray)
    val preStateHash = Blake2b256Hash.fromByteArray(hash.map(_.toByte).toArray)
    BlockRandomSeed(shardId, blockNumber, sender, preStateHash)
  }
  implicit lazy val blockRandomSeedArbitrary = Arbitrary(blockRandomSeedGen)

  // Arbitrary[Blake2b256Hash]
  val blake2b256HashGen = Gen.listOfN(Blake2b256Hash.length, Gen.hexChar).map { hashValue =>
    Blake2b256Hash.fromByteArray(hashValue.map(_.toByte).toArray)
  }
  implicit lazy val blake2b256HashArbitrary = Arbitrary(blake2b256HashGen)

  "encoding BlockRandomSeed object and decoding it" should "give original object" in {
    check { blockRandomSeed: BlockRandomSeed =>
      val serialized = BlockRandomSeed.codecBlockRandomSeed.encode(blockRandomSeed).require
      val restored   = BlockRandomSeed.codecBlockRandomSeed.decode(serialized).require.value

      restored ?= blockRandomSeed
    }
  }

  "encoding Blake2b256Hash object and decoding it" should "give original object" in {
    check { hash: Blake2b256Hash =>
      val serialized = Blake2b256Hash.codecBlake2b256Hash.encode(hash).require
      val restored   = Blake2b256Hash.codecBlake2b256Hash.decode(serialized).require.value

      restored ?= hash
    }
  }

  "generate random seed from constant BlockRandomSeed object" should "give expected value" in {
    val pubKeyStr =
      "0406A102343B05BDF86DF2552C125430CC319323792507328DCC9456713E1E7A24E715171E43ACA3288E41DD346E840901A5D1588C2170AD1D55C0885A3230343A"
    val preStateStr = "AD1356323FFEBE9083687265928AD3CAD1356323FDEBE9083687265928AD3918"

    // Seed input data
    val shardId      = "AD4516"
    val blockNumber  = 1L
    val sender       = PublicKey(fromHex(pubKeyStr).get.toArray)
    val preStateHash = Blake2b256Hash.fromHex(preStateStr)

    // Generated values - seed and first random value
    val (seed, rnd) = {
      val constantBlockRandomSeed = BlockRandomSeed(
        shardId = shardId,
        blockNumber = blockNumber,
        sender = sender,
        preStateHash = preStateHash
      )
      val serialized =
        BlockRandomSeed.codecBlockRandomSeed.encode(constantBlockRandomSeed).require.toByteVector
      val randomNumber =
        ByteVector(BlockRandomSeed.randomGenerator(constantBlockRandomSeed).next())
      (serialized, randomNumber)
    }

    // Reference values as hex strings
    val seedHexRef =
      "06414434353136000000000000000282080d420468760b7bf0dbe4aa5824a86198632646f24a0e651b9928ace27c3cf449ce2a2e3c875946511c83ba68dd0812034ba2b11842e15a3aab8110b4646068755a26ac647ffd7d2106d0e4cb2515a795a26ac647fbd7d2106d0e4cb2515a7230"
    val rndHexRef = "0C338721fa06CAf348C9E013922E8B6D27C6E31FDC2B18FC6A83F144CA22D375"

    // Reference values - seed and first random value
    val (seedRef, rndRef) = (ByteVector.fromHex(seedHexRef).get, ByteVector.fromHex(rndHexRef).get)

    seed shouldBe seedRef
    rnd shouldBe rndRef
  }
}
