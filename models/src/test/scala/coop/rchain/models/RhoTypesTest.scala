package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.BitSetBytesMapper._
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.Serialize
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class RhoTypesTest extends FlatSpec with PropertyChecks with Matchers {

  def rtt[T](msg: T)(implicit serializeInstance: Serialize[T]): Either[Throwable, T] = {
    val bytes = serializeInstance.encode(msg)
    serializeInstance.decode(bytes)
  }

  "Par" should "Pass round-trip serialization" in {
    forAll { par: Par =>
      val result = rtt(par)
      result.right.get should be(par)
    }
  }
}

class BitSetBytesMapperTest extends FlatSpec with PropertyChecks with Matchers {
  "BitSetBytesMapper" should "encode example BitSet-s as expected" in {
    checkMapping(BitSet(), byteString())
    checkMapping(BitSet(0), byteString(1))
    checkMapping(BitSet(1), byteString(2))
    checkMapping(BitSet(0, 1), byteString(3))
    checkMapping(BitSet(7), byteString(-128))
    checkMapping(BitSet(8), byteString(0, 1))
    checkMapping(BitSet(63), byteString(0, 0, 0, 0, 0, 0, 0, -128))
    checkMapping(BitSet(64), byteString(0, 0, 0, 0, 0, 0, 0, 0, 1))
  }

  it should "correctly do an encode-decode round-trip for all possible BitSet-s" in {
    forAll(checkRoundtrip _)
  }

  private def checkMapping(bitSet: BitSet, byteString: ByteString): Assertion = {
    bitSetToByteString(bitSet).toByteArray should be(byteString.toByteArray)
    byteStringToBitSet(byteString) should be(bitSet)
  }

  private def byteString(bytes: Byte*): ByteString = ByteString.copyFrom(Array[Byte](bytes: _*))

  private def checkRoundtrip(bitSet: BitSet): Assertion =
    byteStringToBitSet(bitSetToByteString(bitSet)) should be(bitSet)
}
