package coop.rchain.rspace

import bench.serialization._
import coop.rchain.models._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import scala.reflect.ClassTag

class RoundTripTest extends FlatSpec with PropertyChecks with Matchers {

  def roundTrip[A](in: A)(implicit s: Serialize2ByteBuffer[A]): A = {
    val meta = s.encode(in)
    val out  = s.decode(meta)
    assert(out == in)
    out
  }

  implicit val serialize = KryoSerializers.serializer(classOf[TaggedContinuation])

  def roundTripSerialization[A: Serialize2ByteBuffer: Arbitrary](
      implicit tag: ClassTag[A]
  ): Unit =
    it must s"work for ${tag.runtimeClass.getSimpleName}" in {
      forAll { a: A =>
        roundTrip(a)
      }
    }

  import coop.rchain.models.testImplicits._
  roundTripSerialization[TaggedContinuation]
}
