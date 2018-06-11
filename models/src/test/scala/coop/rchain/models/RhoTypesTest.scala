package coop.rchain.models

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.serialization.implicits._
import testImplicits._
import BitSetBytesMapper._
import coop.rchain.rspace.Serialize

import scala.collection.immutable.BitSet

class RhoTypesTest extends FlatSpec with PropertyChecks with Matchers {

  def rtt[T](msg: T)(implicit serializeInstance: Serialize[T]): Either[Throwable, T] = {
    val bytes = serializeInstance.encode(msg)
    serializeInstance.decode(bytes)
  }

  "Par" should "Pass round-trip serialization" in {
    forAll { (par: Par) =>
      val result = rtt(par)
      result.right.get should be(par)
    }
  }
}

class BitSetBytesMapperTest extends FlatSpec with PropertyChecks with Matchers {
  "BitSetBytesMapper" should "Pass round-trip serialization on empty bitset" in {
    val emptyBitSet = BitSet()
    byteStringToBitSet(bitSetToByteString(emptyBitSet)) should be(emptyBitSet)
  }

  "BitSetBytesMapper" should "Pass round-trip serialization on a long bitset" in {
    val nonEmptyBitSet = BitSet(0, 3, 4, 8, 13, 14, 17, 19, 21, 22, 24, 25, 26, 27, 36, 41, 44, 46,
      47, 50, 51, 53, 56, 57, 58, 60, 63, 65, 66, 67, 68, 70, 72, 88, 910, 911)
    byteStringToBitSet(bitSetToByteString(nonEmptyBitSet)) should be(nonEmptyBitSet)
  }
}
