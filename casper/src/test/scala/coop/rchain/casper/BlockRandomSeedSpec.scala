package coop.rchain.casper

import cats.syntax.all._
import coop.rchain.crypto.PublicKey
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Base16
import coop.rchain.shared.scalatestcontrib.convertToAnyShouldWrapper
import coop.rchain.store.InMemoryKeyValueStore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.Checkers
import scodec.bits.ByteVector
import scodec.bits.ByteVector.fromHex

class BlockRandomSeedSpec extends AnyFlatSpec with Checkers {
  "encoding coop.rchain.casper.BlockRandomSeed object and decoding it" should "give original object" in {
    val blockRandomSeedGen = for {
      shardId                <- Gen.stringOfN(6, Gen.alphaChar)
      blockNumber            <- Gen.choose(0L, 1000000L)
      validatorPublicKeyList <- Gen.listOfN(120, Gen.hexChar)
      validatorPublicKey     = PublicKey(validatorPublicKeyList.map(_.toByte).toArray)
      hash                   <- Gen.listOfN(Blake2b256Hash.length, Gen.hexChar)
      preStateHash           = Blake2b256Hash.fromByteArray(hash.map(_.toByte).toArray)
    } yield BlockRandomSeed(shardId, blockNumber, validatorPublicKey, preStateHash)

    implicit lazy val blockRandomSeedArbitrary = Arbitrary(blockRandomSeedGen)

    check { blockRandomSeed: BlockRandomSeed =>
      val serialized    = BlockRandomSeed.codecBlockRandomSeed.encode(blockRandomSeed).require
      val reconstructed = BlockRandomSeed.codecBlockRandomSeed.decode(serialized).require.value

      reconstructed ?= blockRandomSeed
    }
  }

  "encoding Blake2b256Hash object and decoding it" should "give original object" in {
    val hashGen = for {
      hashValue <- Gen.listOfN(Blake2b256Hash.length, Gen.hexChar)
    } yield Blake2b256Hash.fromByteArray(hashValue.map(_.toByte).toArray)

    implicit lazy val blockRandomSeedArbitrary = Arbitrary(hashGen)

    check { hash: Blake2b256Hash =>
      val serialized = Blake2b256Hash.codecBlake2b256Hash.encode(hash).require
      val restored   = Blake2b256Hash.codecBlake2b256Hash.decode(serialized).require.value

      restored ?= hash
    }
  }

  "generate random seed from constant BlockRandomSeed object" should "give expected value" in {
    val validatorPublicKey = PublicKey(
      fromHex(
        "0406A102343B05BDF86DF2552C125430CC319323792507328DCC9456713E1E7A24E715171E43ACA3288E41DD346E840901A5D1588C2170AD1D55C0885A3230343A"
      ).get.toArray
    )
    val preStateHash =
      Blake2b256Hash.fromHex("AD1356323FFEBE9083687265928AD3CAD1356323FDEBE9083687265928AD3918")

    val constantBlockRandomSeed = BlockRandomSeed(
      shardId = "AD4516",
      blockNumber = 1,
      validatorPublicKey = validatorPublicKey,
      preStateHash = preStateHash
    )

    val serialized =
      BlockRandomSeed.codecBlockRandomSeed.encode(constantBlockRandomSeed).require.toByteVector
    val randomNumber =
      ByteVector(BlockRandomSeed.generateRandomNumber(constantBlockRandomSeed).next())

    val referenceRandomNumber =
      ByteVector.fromHex("0C338721fa06CAf348C9E013922E8B6D27C6E31FDC2B18FC6A83F144CA22D375").get

    val referenceSerialized = ByteVector
      .fromHex(
        "06414434353136000000000000000282080d420468760b7bf0dbe4aa5824a86198632646f24a0e651b9928ace27c3cf449ce2a2e3c875946511c83ba68dd0812034ba2b11842e15a3aab8110b4646068755a26ac647ffd7d2106d0e4cb2515a795a26ac647fbd7d2106d0e4cb2515a7230"
      )
      .get

    serialized shouldBe referenceSerialized
    randomNumber shouldBe referenceRandomNumber
  }
}
