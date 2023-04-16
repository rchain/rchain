package coop.rchain.models

import com.google.protobuf.{ByteString, CodedInputStream}
import coop.rchain.models.Assertions.assertEqual
import coop.rchain.models.BitSetBytesMapper._
import coop.rchain.models.Connective.ConnectiveInstance.{Empty => _}
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.shared.Serialize
import cats.Eval
import org.scalacheck.{Arbitrary, Shrink}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalapb.GeneratedMessageCompanion
import coop.rchain.catscontrib.effect.implicits.sEval

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
// TODO This fails if minSuccessful > 2, sizeRange > 10 which is odd.
//  But this will be irrelevant with manual Rho types so leave this commented out and remove once
//  tests for manual types are implemented.
//class RhoTypesTest extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
//
//  //FIXME crank that up and fix the resulting errors
//  implicit override val generatorDrivenConfig =
//    PropertyCheckConfiguration(minSuccessful = 50, sizeRange = 250)
//
//  behavior of "Round-trip serialization"
//
//  roundTripSerialization[Par]
//  roundTripSerialization[Expr]
//  roundTripSerialization[Var]
//  roundTripSerialization[Send]
//  roundTripSerialization[Receive]
//  roundTripSerialization[New]
//  roundTripSerialization[Match]
//  roundTripSerialization[ESet]
//  roundTripSerialization[EMap]
//
//  def roundTripSerialization[A <: StacksafeMessage[A]: Serialize: Arbitrary: Shrink: Pretty](
//      implicit tag: ClassTag[A],
//      companion: GeneratedMessageCompanion[A]
//  ): Unit =
//    it must s"work for ${tag.runtimeClass.getSimpleName}" in {
//      forAll { a: A =>
//        roundTripSerialization(a)
//        stacksafeSizeSameAsReference(a)
//        stacksafeWriteToSameAsReference(a)
//        stacksafeReadFromSameAsReference(a)
//      }
//    }
//
//  def roundTripSerialization[A: Serialize: Pretty](a: A): Assertion = {
//    val bytes    = Serialize[A].encode(a)
//    val result   = Serialize[A].decode(bytes)
//    val expected = Right(a)
//    assertEqual(result, expected)
//  }
//
//  def stacksafeSizeSameAsReference[A <: StacksafeMessage[A]](a: A): Assertion =
//    assert(ProtoM.serializedSize(a).value == a.serializedSize)
//
//  def stacksafeWriteToSameAsReference[A <: StacksafeMessage[A]](a: A): Assertion =
//    assert(ProtoM.toByteArray(a).value sameElements a.toByteArray)
//
//  def stacksafeReadFromSameAsReference[A <: StacksafeMessage[A]](a: A)(
//      implicit companion: GeneratedMessageCompanion[A]
//  ): Assertion = {
//    // We don't want to rely on the (sometimes overridden) equals,
//    // so instead we compare the output of reference serializer
//    // on the messages parsed by the stacksafe deserializer.
//    // In short we check `referenceSerialize === referenceSerialize . stacksafeDeserialize . referenceSerialize`.
//    // We also check `stacksafeDeserialize(referenceSerialize(a)).equals(a)` just in case :)
//    val referenceBytes = a.toByteArray
//    val in             = CodedInputStream.newInstance(referenceBytes)
//    val decoded        = companion.defaultInstance.mergeFromM[Eval](in).value
//    val encoded        = decoded.toByteArray
//    assert(encoded sameElements referenceBytes)
//    assert(decoded == a)
//  }
//
//}
//
//class BitSetBytesMapperTest extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
//  "BitSetBytesMapper" should "encode example BitSet-s as expected" in {
//    checkMapping(BitSet(), byteString())
//    checkMapping(BitSet(0), byteString(1))
//    checkMapping(BitSet(1), byteString(2))
//    checkMapping(BitSet(0, 1), byteString(3))
//    checkMapping(BitSet(7), byteString(-128))
//    checkMapping(BitSet(8), byteString(0, 1))
//    checkMapping(BitSet(63), byteString(0, 0, 0, 0, 0, 0, 0, -128))
//    checkMapping(BitSet(64), byteString(0, 0, 0, 0, 0, 0, 0, 0, 1))
//  }
//
//  it should "correctly do an encode-decode round-trip for all possible BitSet-s" in {
//    forAll(checkRoundtrip _)
//  }
//
//  private def checkMapping(bitSet: BitSet, byteString: ByteString): Assertion = {
//    bitSetToByteString(bitSet).toByteArray should be(byteString.toByteArray)
//    byteStringToBitSet(byteString) should be(bitSet)
//  }
//
//  private def byteString(bytes: Byte*): ByteString = ByteString.copyFrom(Array[Byte](bytes: _*))
//
//  private def checkRoundtrip(bitSet: BitSet): Assertion =
//    byteStringToBitSet(bitSetToByteString(bitSet)) should be(bitSet)
//}
