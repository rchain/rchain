package coop.rchain.rspace

import coop.rchain.models._
import org.scalacheck._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._

import scala.reflect.ClassTag

class KryoRoundTripTest extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50, sizeRange = 250)

  implicit val exprSerialize               = KryoSerializers.serializer(classOf[Expr])
  implicit val unfSerialize                = KryoSerializers.serializer(classOf[GUnforgeable])
  implicit val parSerialize                = KryoSerializers.serializer(classOf[Par])
  implicit val bindPatternSerialize        = KryoSerializers.serializer(classOf[BindPattern])
  implicit val listParWithRandomSerialize  = KryoSerializers.serializer(classOf[ListParWithRandom])
  implicit val taggedContinuationSerialize = KryoSerializers.serializer(classOf[TaggedContinuation])

  def roundTrip[A](in: A)(implicit s: Serialize2ByteBuffer[A]): Assertion = {
    val meta = s.encode(in)
    val out  = s.decode(meta)
    assert(out == in)
  }

  def roundTripSerialization[A: Arbitrary: Shrink](
      implicit s: Serialize2ByteBuffer[A],
      tag: ClassTag[A]
  ): Unit =
    it must s"work for ${tag.runtimeClass.getSimpleName}" in {
      forAll { a: A =>
        roundTrip(a)
      }
    }

  import coop.rchain.models.testImplicits._
  roundTripSerialization[Expr]
  roundTripSerialization[Par]
  roundTripSerialization[BindPattern]
  roundTripSerialization[ListParWithRandom]
  roundTripSerialization[TaggedContinuation]
}
