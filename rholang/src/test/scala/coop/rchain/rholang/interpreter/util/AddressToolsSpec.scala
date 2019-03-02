package coop.rchain.rholang.interpreter.util
import coop.rchain.crypto.hash.Blake2b256
import org.scalacheck._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}
import cats.implicits._
import coop.rchain.rholang.interpreter.util.codec.Base58

class AddressToolsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit val propertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000, sizeRange = 200)

  def genByteArrayOf(n: Int) = Gen.listOfN(n, Arbitrary.arbByte.arbitrary).map(_.toArray)

  val genArgs =
    for {
      prefixLength   <- Gen.posNum[Int]
      keyLength      <- Gen.posNum[Int]
      checksumLength <- Gen.choose(0, Blake2b256.hashLength)
      prefix         <- genByteArrayOf(prefixLength)
      publicKey      <- genByteArrayOf(keyLength)
    } yield (prefix, keyLength, checksumLength, publicKey)

  property("parse after fromPublicKey works correctly") {

    forAll(genArgs) {
      case (prefix, keyLength, checksumLength, pk) =>
        val tools = new AddressTools(prefix, keyLength, checksumLength)

        val address = tools.fromPublicKey(pk).get

        val parsedAddress = tools.parse(address).right.get

        parsedAddress.prefix should be(prefix.deep)
        parsedAddress.keyHash should be(Blake2b256.hash(pk))
    }
  }

  property("replacing a character in an address makes the parsing fail") {
    val generator =
      for {
        args <- genArgs

        (prefix, keyLength, checksumLength, pk) = args

        tools = new AddressTools(prefix, keyLength, checksumLength)

        address = tools.fromPublicKey(pk).get

        idx <- Gen.choose(0, address.length - 1)

        badChar <- Arbitrary.arbChar.arbitrary.filter(c => address(idx) != c)

      } yield (tools, address, idx, badChar)

    forAll(generator) {
      case (tools, address, idx, newChar) =>
        val badAddress = address.substring(0, idx) + newChar + address.substring(idx)

        tools.parse(badAddress).isLeft should be(true)
    }
  }

  property("if the key length is different than the expected key size expect an error") {
    val generator =
      for {
        args        <- genArgs
        badKeyDelta <- Arbitrary.arbInt.arbitrary
      } yield (args, badKeyDelta)

    forAll(generator) {
      case ((prefix, correctKeyLength, checksumLength, pk), badKeyDelta) =>
        val tools           = new AddressTools(prefix, correctKeyLength + badKeyDelta, checksumLength)
        val expectedSuccess = badKeyDelta == 0
        tools.fromPublicKey(pk).isDefined should be(expectedSuccess)
    }
  }
}
