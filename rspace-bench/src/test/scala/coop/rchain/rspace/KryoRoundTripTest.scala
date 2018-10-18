package coop.rchain.rspace

import bench.serialization._
import coop.rchain.models._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import scala.reflect.ClassTag

class KryoRoundTripTest extends FlatSpec with PropertyChecks with Matchers {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50, sizeRange = 250)

  implicit val exprSerialize               = KryoSerializers.serializer(classOf[Expr])
  implicit val parSerialize                = KryoSerializers.serializer(classOf[Par])
  implicit val bindPatternSerialize        = KryoSerializers.serializer(classOf[BindPattern])
  implicit val listParWithRandomSerialize  = KryoSerializers.serializer(classOf[ListParWithRandom])
  implicit val taggedContinuationSerialize = KryoSerializers.serializer(classOf[TaggedContinuation])

  def roundTripSerialization[A: Arbitrary](
      implicit s: Serialize2ByteBuffer[A],
      tag: ClassTag[A]
  ): Unit =
    it must s"work for ${tag.runtimeClass.getSimpleName}" in {
      forAll { a: A =>
        s.roundTrip(a)
      }
    }

  import coop.rchain.models.testImplicits._
  roundTripSerialization[Expr]
  roundTripSerialization[Par]
  roundTripSerialization[BindPattern]
  roundTripSerialization[ListParWithRandom]
  roundTripSerialization[TaggedContinuation]
}
