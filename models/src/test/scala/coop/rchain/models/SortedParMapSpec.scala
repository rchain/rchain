package coop.rchain.models

import java.util

import com.google.protobuf.ByteString
import coop.rchain.models.Assertions.assertEqual
import coop.rchain.models.Expr.ExprInstance.{GInt, _}
import coop.rchain.models.Var.VarInstance.BoundVar
import coop.rchain.models.rholang.implicits._
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class SortedParMapSpec extends FlatSpec with Matchers {

  private[this] def toKVpair(pair: (Par, Par)): KeyValuePair = KeyValuePair(pair._1, pair._2)

  private[this] def serializeEMap(map: SortedParMap): Array[Byte] =
    EMap(map.sortedMap.map(toKVpair).toSeq).toByteArray

  val pars: Seq[(Par, Par)] = Seq[(Par, Par)](
    (GInt(7), GString("Seven")),
    (GInt(7), GString("SeVen")),
    (EVar(BoundVar(1)), EVar(BoundVar(0))),
    (GInt(2), ParSet(Seq[Par](GInt(2), GInt(1)))),
    (GInt(2), ParSet(Seq[Par](GInt(2))))
  )

  private def roundTripTest(parMap: SortedParMap): Assertion =
    EMap.parseFrom(serializeEMap(parMap)) should ===(EMap(parMap.sortedMap.map(toKVpair).toSeq))

  def sample = new SortedParMap(pars.toMap)

  "SortedParMap" should "preserve structure during round trip protobuf serialization" in {
    roundTripTest(sample)
  }

  it should "deduplicate elements where last seen element wins" in {
    val expectedPairs: Map[Par, Par] = Map(
      (GInt(7), GString("SeVen")),
      (GInt(2), ParSet(Seq[Par](GInt(2))))
    )

    val afterRoundtripSerialization = EMap.parseFrom(serializeEMap(sample)).kvs
    val result = expectedPairs.forall {
      case (key, value) =>
        afterRoundtripSerialization.find(_.key == key).get.value == value
    }
    result should be(true)
  }

  it should "preserve ordering during serialization" in {
    val referenceBytes = serializeEMap(sample)

    (1 to 1000).forall { i =>
      withClue(s"Run #$i serialization: ") {
        val serialized = serializeEMap(sample)
        val res        = util.Arrays.equals(serialized, referenceBytes)
        assert(res == true, ". Same set of Pars should serialize deterministically")
        res
      }
    } should be(true)
  }

  it should "be equal when it is equal" in {
    val ps = Map(
      (
        Par(),
        Par(
          List(),
          List(),
          List(),
          List(),
          List(),
          List(GPrivate(ByteString.copyFrom(Array[Byte](0))), GPrivate(ByteString.EMPTY)),
          List(),
          List(),
          AlwaysEqual(BitSet()),
          true
        )
      )
    )
    assertEqual(new SortedParMap(ps), new SortedParMap(ps))
  }
}
