package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.Assertions.assertEqual
import coop.rchain.models.BitSetBytesMapper._
import coop.rchain.models.Connective.ConnectiveInstance.{Empty => _}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.Serialize
import org.scalacheck.{Arbitrary, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

class RhoTypesTest extends FlatSpec with PropertyChecks with Matchers {

  //FIXME crank that up and fix the resulting errors
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50, sizeRange = 250)

  behavior of "Round-trip serialization"

  roundTripSerialization[Par]
  roundTripSerialization[Expr]
  roundTripSerialization[Var]
  roundTripSerialization[Send]
  roundTripSerialization[Receive]
  roundTripSerialization[New]
  roundTripSerialization[Match]
  roundTripSerialization[ESet]
  roundTripSerialization[EMap]

  def isExcluded(a: Any): Boolean = {
    //This won't prevent such cases from being generated
    //within the deeply-nested ast trees, but will make it much less likely.
    //FIXME eradicate.
    val exclusions: PartialFunction[Any, Boolean] = {
      case Expr(EMapBody(_)) => true
    }
    exclusions.applyOrElse(a, { x: Any =>
      false
    })
  }

  def roundTripSerialization[A: Serialize: Arbitrary: Shrink: Pretty](implicit tag: ClassTag[A]): Unit =
    it must s"work for ${tag.runtimeClass.getSimpleName}" in {
      forAll { a: A =>
        whenever(!isExcluded(a)) {
          roundTripSerialization(a)
        }
      }
    }

  def roundTripSerialization[A: Serialize: Arbitrary: Shrink: Pretty](a: A): Assertion = {
    val bytes    = Serialize[A].encode(a)
    val result   = Serialize[A].decode(bytes)
    val expected = Right(a)
    assertEqual(result, expected)
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
