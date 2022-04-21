package coop.rchain.models

import java.util

import com.google.protobuf.ByteString
import coop.rchain.models.Assertions.assertEqual
import coop.rchain.models.Connective.ConnectiveInstance.ConnBool
import coop.rchain.models.Expr.ExprInstance.{GInt, _}
import coop.rchain.models.Var.VarInstance.BoundVar
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.models.rholang.sorter.ordering._
import coop.rchain.models.testImplicits._
import coop.rchain.models.testUtils.TestUtils.sort
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

class SortedParMapSpec extends FlatSpec with PropertyChecks with Matchers {

  private[this] def toKVpair(pair: (Par, Par)): KeyValuePair = KeyValuePair(pair._1, pair._2)

  private[this] def serializeEMap(map: SortedParMap): Array[Byte] =
    EMap(map.sortedList.map(toKVpair)).toByteArray

  val pars: Seq[(Par, Par)] = Seq[(Par, Par)](
    (GInt(7), GString("Seven")),
    (GInt(7), GString("SeVen")),
    (EVar(BoundVar(1)), EVar(BoundVar(0))),
    (GInt(2), ParSet(Seq[Par](GInt(2), GInt(1)))),
    (GInt(2), ParSet(Seq[Par](GInt(2))))
  )

  private def roundTripTest(parMap: SortedParMap): Assertion =
    EMap.parseFrom(serializeEMap(parMap)) should ===(EMap(parMap.sortedList.map(toKVpair)))

  def sample = SortedParMap(pars)

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
      Par() -> Par(
        unforgeables = Seq(
          GPrivate(
            ByteString.copyFrom(
              Array[Byte](
                0
              )
            )
          ),
          GPrivate()
        ),
        connectiveUsed = true
      )
    )
    assertEqual(SortedParMap(ps), SortedParMap(ps))
  }

  it should "keep keys sorted" in {
    val sortedParMapKeys = List(
      Par(
        connectiveUsed = true
      ),
      Par(
        bundles = Seq(
          Bundle(
            writeFlag = true
          )
        )
      ),
      Par(
        connectives = Seq(
          Connective(
            ConnBool(
              false
            )
          )
        )
      ),
      Par(
        unforgeables = Seq(
          GPrivate()
        )
      ),
      Par()
    )
    val sortedParMap = SortedParMap(sortedParMapKeys.zip(Seq.fill(sortedParMapKeys.size)(Par())))
    val keys         = sortedParMap.sortedKeys
    assert(keys.toList == keys.toList.sort)
  }

  it should "sort all input" in {
    forAll { (keys: Seq[Par], values: Seq[Par]) =>
      val kvs = keys.zip(values)
      val map = SortedParMap(kvs)

      kvs.headOption
        .map(_._1)
        .foreach(
          (unsorted: Par) => {
            val sorted = sort(unsorted)
            checkSortedInput(map.-, unsorted, sorted)
            checkSortedInput(map.apply, unsorted, sorted)
            checkSortedInput(map.contains, unsorted, sorted)
            checkSortedInput(map.get, unsorted, sorted)
            checkSortedInput(map.getOrElse(_: Par, Par()), unsorted, sorted)
          }
        )
      checkSortedInput(map.--(_: Seq[Par]), keys, keys.toList.sort)
    }
  }

  it should "preserve sortedness of all elements and the whole map for all its operations" in {
    forAll { (pars1: Seq[Par], pars2: Seq[Par]) =>
      val map1 = SortedParMap(pars1.zip(pars2))
      val map2 = SortedParMap(pars2.zip(pars1))
      checkSorted(map1)
      checkSorted(map2)
      Seq(map1.headOption, map2.headOption).flatten.foreach(kv => {
        checkSorted(map1 + kv)
        checkSorted(map2 - kv._1)
        checkSorted(map2 + kv)
        checkSorted(map1 - kv._1)
      })
    }
  }

  private def checkSorted(sortedParMap: SortedParMap): Unit = {
    checkSorted(sortedParMap.sortedKeys)
    sortedParMap.sortedValues.foreach(p => assert(isSorted(p)))
    sortedParMap.foreach(kv => {
      assert(isSorted(kv._1))
      assert(isSorted(kv._2))
      assert(isSorted(sortedParMap(kv._1)))
    })
  }

  private def checkSorted(iterable: Iterable[Par]) = {
    import coop.rchain.models.rholang.sorter.ordering._
    iterable.foreach(p => assert(isSorted(p)))
    assert(iterable.toList == iterable.toList.sort)
  }

  private def checkSortedInput[A, B](f: A => B, unsorted: A, sorted: A): Assertion =
    assert(f(sorted) == f(unsorted))

  def isSorted[A: Sortable](a: A): Boolean =
    a == Sortable[A].sortMatch[Coeval](a).value().term
}
