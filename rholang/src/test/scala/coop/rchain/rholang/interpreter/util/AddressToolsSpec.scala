package coop.rchain.rholang.interpreter.util
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import org.scalacheck._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import coop.rchain.crypto.PublicKey
import coop.rchain.rholang.interpreter.util.codec.Base58
import coop.rchain.shared.{Base16, EqualitySpecUtils}

class AddressToolsSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit val propertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000, sizeRange = 200)

  private def genByteArrayOf(n: Int) = Gen.listOfN(n, Arbitrary.arbByte.arbitrary).map(_.toArray)

  private val genArgs =
    for {
      prefixLength   <- Gen.posNum[Int]
      keyLength      <- Gen.posNum[Int]
      checksumLength <- Gen.choose(0, Blake2b256.hashLength)
      prefix         <- genByteArrayOf(prefixLength)
      publicKey      <- genByteArrayOf(keyLength)
    } yield (prefix, keyLength, checksumLength, PublicKey(publicKey))

  property("defines value-based equality") {
    EqualitySpecUtils.checkValueBasedEquality(
      Address(Array(), Array(), Array()) ::
        Address(Array(1), Array(), Array()) ::
        Address(Array(), Array(2), Array()) ::
        Address(Array(), Array(), Array(3)) ::
        Nil
    )
  }

  property("parse after fromPublicKey works correctly") {

    forAll(genArgs) {
      case (prefix, keyLength, checksumLength, pk) =>
        val tools = new AddressTools(prefix, keyLength, checksumLength)

        val address = tools.fromPublicKey(pk).get.toBase58

        val parsedAddress = tools.parse(address).right.get

        val ethAddress = Base16.encode(Keccak256.hash(pk.bytes.drop(1))).takeRight(40)
        val keyHash    = Keccak256.hash(Base16.unsafeDecode(ethAddress))

        parsedAddress.prefix should be(prefix.deep)
        parsedAddress.keyHash should be(keyHash)
    }
  }

  property("replacing a character in an address makes the parsing fail") {
    val generator =
      for {
        args <- genArgs

        (prefix, keyLength, checksumLength, pk) = args

        tools = new AddressTools(prefix, keyLength, checksumLength)

        address = tools.fromPublicKey(pk).get.toBase58

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
