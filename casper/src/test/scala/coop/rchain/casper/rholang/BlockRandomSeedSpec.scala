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

  // If random generator seed logic changes, node will not able to replay previous blocks
  "BlockRandomSeed serializer and random generator" should "not change without hard-fork" in {
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
    val seedHexExpected =
      "0641443435313601410406a102343b05bdf86df2552c125430cc319323792507328dcc9456713e1e7a24e715171e43aca3288e41dd346e840901a5d1588c2170ad1d55c0885a3230343aad1356323ffebe9083687265928ad3cad1356323fdebe9083687265928ad3918"
    val rndHexExpected = "5d92fa794d3e68dcbdc0716de2b5799f95d04ef122109e21cb42f5fd7d64b359"

    // Reference values - seed and first random value
    val (seedExpected, rndExpected) =
      (ByteVector.fromHex(seedHexExpected).get, ByteVector.fromHex(rndHexExpected).get)

    seed shouldBe seedExpected
    // Checking of random number is not part of the seed logic so it's here just debugging information
    rnd shouldBe rndExpected
  }
}
