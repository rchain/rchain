package coop.rchain.rholang.interpreter.accounting

import cats.effect.IO
import com.google.protobuf.ByteString
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting.Chargeable._
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import org.scalactic.TripleEqualsSupport
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

import java.nio.file.{Files, Path}
import scala.collection.immutable.BitSet
import scala.concurrent.duration._
import coop.rchain.shared.RChainScheduler._

class RholangMethodsCostsSpec
    extends AnyWordSpec
    with TripleEqualsSupport
    with Matchers
    with BeforeAndAfterAll {

  "nth method" when {
    "called on a list" should {
      "charge constant cost regardless of list size or value of n" in {
        val table = Table(
          ("list", "index"),
          (listN(1), 0L),
          (listN(10), 9L),
          (listN(100), 99L)
        )
        forAll(table) { (pars: Vector[Par], n: Long) =>
          val method = methodCall("nth", EList(pars), List(GInt(n)))
          test(method, NTH_METHOD_CALL_COST)
        }
      }

      "charge also when index is out of bound" in {
        val table = Table(
          ("list", "index"),
          (listN(0), 1L)
        )
        forAll(table) { (pars, n) =>
          implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync
          implicit val env  = Env[Par]()
          val method        = methodCall("nth", EList(pars), List(GInt(n)))
          withReducer[Assertion] { reducer =>
            for {
              err  <- reducer.evalExprToPar(method).attempt
              _    = assert(err.isLeft)
              cost <- methodCallCost(reducer)
            } yield assert(cost.value === 10)
          }
        }
      }
    }

    "called on tuples" should {
      "charge constant cost regardless of list size or value of n" in {
        val table = Table(
          ("list", "index"),
          (listN(1), 0L),
          (listN(10), 9L),
          (listN(100), 99L)
        )
        forAll(table) { (pars: Vector[Par], n: Long) =>
          val method = methodCall("nth", EList(pars), List(GInt(n)))
          test(method, NTH_METHOD_CALL_COST)
        }
      }

      "charge also when index is out of bound" in {
        val table = Table(
          ("list", "index"),
          (listN(0), 1L)
        )
        forAll(table) { (pars, n) =>
          implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync
          implicit val env  = Env[Par]()
          val method        = methodCall("nth", EList(pars), List(GInt(n)))
          withReducer[Assertion] { reducer =>
            for {
              err  <- reducer.evalExprToPar(method).attempt
              _    = assert(err.isLeft)
              cost <- methodCallCost(reducer)
            } yield assert(cost.value === 10)
          }
        }
      }
    }
  }

  def costIsProportional(base: Cost, factor: Double, expected: Cost): Assertion =
    (base.value * factor) shouldBe expected.value.toDouble

  def testProportional[A](
      baseCost: Cost,
      factor: Double,
      method: Expr
  ): Assertion = {
    implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync
    implicit val env  = Env[Par]()
    withReducer { reducer =>
      for {
        _    <- reducer.evalExprToPar(method)
        cost <- methodCallCost(reducer)
      } yield costIsProportional(baseCost, factor, cost)
    }
  }

  "toByteArray" when {
    "called on Rholang term" should {
      "charge proportionally to the byte size of the underlying term" in {
        val pars = Table[Par](
          "par",
          Par(exprs = Seq(GInt(1))),
          Send(GString("result"), List(GString("Success")), false, BitSet()),
          Receive(
            Seq(
              ReceiveBind(Seq(Par()), Bundle(GString("y"), readFlag = false, writeFlag = true))
            ),
            Par()
          ),
          EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("Alice"))))),
          GString("Hello"),
          gBigInt("-9999999999999999999999999999999999999999")
        )
        val baseTerm = Par(exprs = Seq(GInt(1)))
        val baseCost = Cost(baseTerm)
        forAll(pars) { par =>
          val substitutionCost = Cost(par)
          val method           = methodCall("toByteArray", par, List.empty)
          val factor           = par.serializedSize.toDouble / baseTerm.serializedSize
          // We substitute the target term before evaluating toByteArray method on it.
          // Cost of substitution is proportional to the byte size of the term.
          val c = Cost((baseCost.value + (substitutionCost.value / factor)).toLong)
          testProportional(c, factor, method)
        }
      }
    }
  }

  "hexToBytes" when {
    "called on hex encoded string" should {
      "charge proportionally to the length of the string" in {
        val strings = Table(
          "a",
          "a1231",
          "abcdef",
          Seq.fill(1000)("a").mkString("")
        )
        val base     = "a"
        val baseCost = hexToBytesCost(base)
        forAll(strings) { str =>
          val encodedStr = new String(str.getBytes("UTF-8"))
          val method     = methodCall("hexToBytes", GString(encodedStr), List.empty)
          val factor     = str.length.toDouble / base.length
          testProportional(
            baseCost,
            factor,
            method
          )
        }
      }
    }
  }

  "bytesToHex" when {
    "called on byte array" should {
      "charge proportionally to the length of the bytes" in {
        val byteArrays = Table[Array[Byte]](
          ("Bytes"),
          Array.ofDim[Byte](1),
          Array.ofDim[Byte](3),
          Array.ofDim[Byte](7),
          Array.ofDim[Byte](10)
        )
        val base     = Array.ofDim[Byte](1)
        val baseCost = bytesToHexCost(base)
        forAll(byteArrays) { bytes =>
          val decodedBytes = GByteArray(ByteString.copyFrom(bytes))
          val method       = methodCall("bytesToHex", decodedBytes, List.empty)
          val factor       = bytes.length.toDouble / base.length
          testProportional(
            baseCost,
            factor,
            method
          )
        }
      }
    }
  }

  "toUtf8Bytes" when {
    "called on String" should {
      "charge proportionally to the length of the String" in {
        val utf8Strings = Table(
          "string",
          "a",
          "",
          "abcd",
          Seq.fill(100)("a").mkString("")
        )

        val refString = "a"
        val refCost   = hexToBytesCost(refString)
        forAll(utf8Strings) { string =>
          val factor = string.length.toDouble / refString.size
          val method = methodCall("toUtf8Bytes", GString(string), List.empty)
          testProportional(refCost, factor, method)
        }
      }
    }
  }

  "union" when {
    "called on Map" should {
      "charge proportionally to the size of the argument Map" in {
        val maps = Table(
          ("baseMap", "argumentMap"),
          (emptyMap, emptyMap),
          (emptyMap, mapN(1, GString("two"))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        val refMap   = mapN(1, GInt(1))
        val baseCost = unionCost(refMap.size)
        forAll(maps) {
          case (baseMap, argMap) =>
            val factor = argMap.size.toDouble / refMap.size
            val method = methodCall("union", toRholangMap(baseMap), List(toRholangMap(argMap)))
            testProportional(baseCost, factor, method)
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Set" in {
        val sets = Table(
          ("baseSet", "argumentSet"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        val unitSet  = setN(1)
        val baseCost = unionCost(unitSet.size)
        forAll(sets) {
          case (baseSet, argSet) =>
            val factor = argSet.size.toDouble / unitSet.size
            val method = methodCall("union", toRholangSet(baseSet), List(toRholangSet(argSet)))
            testProportional(baseCost, factor, method)
        }
      }
    }
  }

  "diff" when {
    "called on Map" should {
      "charge proportionally to the size of the argument Map" in {
        val maps = Table(
          ("baseMap", "argumentMap"),
          (emptyMap, emptyMap),
          (emptyMap, mapN(1, GString("two"))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        val refArg   = mapN(1, GString("two"))
        val baseCost = diffCost(refArg.size)

        forAll(maps) {
          case (baseMap, argMap) =>
            val method = methodCall("diff", toRholangMap(baseMap), List(toRholangMap(argMap)))
            val factor = argMap.size.toDouble / refArg.size
            testProportional(baseCost, factor, method)
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Set" in {
        val maps = Table(
          ("baseSet", "argumentSet"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        val refArg   = setN(1)
        val baseCost = diffCost(refArg.size)
        forAll(maps) {
          case (baseSet, argSet) =>
            val method = methodCall("diff", toRholangSet(baseSet), List(toRholangSet(argSet)))
            val factor = argSet.size.toDouble / refArg.size
            testProportional(baseCost, factor, method)
        }
      }
    }
  }

  "add" when {
    "called on Set" should {
      "charge constant cost" in {
        val sets = Table[Set[Par], Par](
          ("set", "elem"),
          (emptySet, Par()),
          (emptySet, GInt(1)),
          (setN(10), GString("test")),
          (setN(1), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(sets) {
          case (set, elem) =>
            test(methodCall("add", toRholangSet(set), List(elem)), ADD_COST)
        }
      }
    }
  }

  "delete" when {
    "called on Set" should {
      "charge constant cost" in {
        val sets = Table[Set[Par], Par](
          ("set", "elem"),
          (emptySet, Par()),
          (Set(GInt(1)), GInt(1)),
          (setN(100), GInt(99)),
          (setN(1), toRholangMap(mapN(10, GInt(10)))),
          (Set(toRholangMap(mapN(10, GInt(10)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(sets) {
          case (set, elem) =>
            test(methodCall("delete", toRholangSet(set), List(elem)), REMOVE_COST)
        }
      }
    }

    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par], Par](
          ("map", "elem"),
          (emptyMap, Par()),
          (mapN(1, GInt(1)), GInt(1)),
          (mapN(100, GInt(1)), GInt(99)),
          (mapN(1, GInt(1)), toRholangMap(mapN(10, GInt(10)))),
          (map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(maps) {
          case (map, elem) =>
            test(methodCall("delete", toRholangMap(map), List(elem)), REMOVE_COST)
        }
      }
    }
  }

  "contains" when {
    "called on Set" should {
      "charge constant cost" in {
        val sets = Table[Set[Par], Par](
          ("set", "elem"),
          (emptySet, Par()),
          (Set(GInt(1)), GInt(1)),
          (setN(100), GInt(99)),
          (setN(1), toRholangMap(mapN(10, GInt(10)))),
          (Set(toRholangMap(mapN(10, GInt(10)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(sets) {
          case (set, elem) =>
            test(methodCall("contains", toRholangSet(set), List(elem)), LOOKUP_COST)
        }
      }
    }

    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par], Par](
          ("map", "elem"),
          (emptyMap, Par()),
          (mapN(1, GInt(1)), GInt(1)),
          (mapN(100, GInt(1)), GInt(99)),
          (mapN(1, GInt(1)), toRholangMap(mapN(10, GInt(10)))),
          (map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(maps) {
          case (map, elem) =>
            test(methodCall("contains", toRholangMap(map), List(elem)), LOOKUP_COST)
        }
      }
    }
  }

  "get" when {
    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par], Par](
          ("map", "elem"),
          (emptyMap, Par()),
          (mapN(1, GInt(1)), GInt(1)),
          (mapN(100, GInt(1)), GInt(99)),
          (mapN(1, GInt(1)), toRholangMap(mapN(10, GInt(10)))),
          (map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(maps) {
          case (map, elem) =>
            test(methodCall("get", toRholangMap(map), List(elem)), LOOKUP_COST)
        }
      }
    }
  }

  "getOrElse" when {
    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par], Par](
          ("map", "elem"),
          (emptyMap, Par()),
          (mapN(1, GInt(1)), GInt(1)),
          (mapN(100, GInt(1)), GInt(99)),
          (mapN(1, GInt(1)), toRholangMap(mapN(10, GInt(10)))),
          (map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(maps) {
          case (map, elem) =>
            test(methodCall("getOrElse", toRholangMap(map), List(elem, Par())), LOOKUP_COST)
        }
      }
    }
  }

  "set" when {
    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par], Par](
          ("map", "elem"),
          (emptyMap, Par()),
          (mapN(1, GInt(1)), GInt(1)),
          (mapN(100, GInt(1)), GInt(99)),
          (mapN(1, GInt(1)), toRholangMap(mapN(10, GInt(10)))),
          (map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1)))), toRholangMap(mapN(10, GInt(10))))
        )
        forAll(maps) {
          case (map, elem) =>
            test(methodCall("set", toRholangMap(map), List(elem, Par())), LOOKUP_COST)
        }
      }
    }
  }

  "keys" when {
    "called on Map" should {
      "charge constant cost" in {
        val maps = Table[Map[Par, Par]](
          "map",
          emptyMap,
          mapN(1, GInt(1)),
          mapN(100, GInt(1)),
          map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1))))
        )
        forAll(maps) { map =>
          test(methodCall("keys", toRholangMap(map), List()), KEYS_METHOD_COST)
        }
      }
    }
  }

  "size" when {
    "called on Map" should {
      "charge proportionally to the number of elements in the base Map" in {
        val maps = Table[Map[Par, Par]](
          "map",
          emptyMap,
          mapN(1, GInt(1)),
          mapN(100, GInt(1)),
          map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1))))
        )
        val refMap   = mapN(1, GInt(1))
        val baseCost = sizeMethodCost(refMap.size)
        forAll(maps) { map =>
          val method = methodCall("size", toRholangMap(map), List())
          val factor = map.size.toDouble / refMap.size
          testProportional(baseCost, factor, method)
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the number of elements in the base Set" in {
        val sets = Table[Set[Par]](
          "set",
          emptySet,
          setN(1),
          setN(100),
          Set(toRholangMap(mapN(10, GInt(10))))
        )
        val refSet   = setN(3)
        val baseCost = sizeMethodCost(refSet.size)
        forAll(sets) { set =>
          val method = methodCall("size", toRholangSet(set), List())
          val factor = set.size.toDouble / refSet.size
          testProportional(baseCost, factor, method)
        }
      }
    }
  }

  "length" when {
    "called on List" should {
      "charge constant cost" in {
        val lists = Table[Vector[Par]](
          "list",
          Vector.empty[Par],
          Vector[Par](GInt(1)),
          setN(100).toVector,
          Vector[Par](toRholangMap(mapN(10, GInt(10))))
        )
        forAll(lists) { vector =>
          test(methodCall("length", toRholangList(vector), List()), LENGTH_METHOD_COST)
        }
      }
    }

    "called on String" should {
      "charge constant cost" in {
        val strings = Table[GString](
          "string",
          GString(""),
          GString("abcd"),
          GString(Seq.fill(10000)("").mkString)
        )
        forAll(strings) { string =>
          test(methodCall("length", Par(exprs = Seq(string)), List()), OP_CALL_COST)
        }
      }
    }
  }

  "slice" when {
    "called on List" should {
      "charge proportionally to the number of elements it has to traverse" in {
        val lists = Table[Vector[Par], (Long, Long)](
          ("list", "slice-args"),
          (Vector.empty[Par], (0L, 1L)),
          (Vector.empty[Par], (1L, 0L)),
          (Vector[Par](GInt(1L)), (0L, 1L)),
          (setN(100).toVector, (10L, 20L)),
          (Vector[Par](toRholangMap(mapN(10, GInt(10L)))), (0L, 1L))
        )
        val refSlice = (0, 1)
        val baseCost = sliceCost(refSlice._2)
        forAll(lists) {
          case (list, (from, to)) =>
            val method = methodCall("slice", toRholangList(list), List(GInt(from), GInt(to)))
            val factor = to.toDouble / refSlice._2
            testProportional(baseCost, factor, method)
        }
      }
    }

    "called on String" should {
      "charge proportionally to the number of elements it has to traverse" in {
        val strings = Table[String, (Long, Long)](
          ("string", "slice-args"),
          ("", (0L, 0L)),
          ("", (1L, 0L)),
          ("abcd", (2L, 4L)),
          (Seq.fill(100)("").mkString, (10L, 100L))
        )
        val refSlice = (0, 1)
        val baseCost = sliceCost(refSlice._2)
        forAll(strings) {
          case (string, (from, to)) =>
            val method =
              methodCall("slice", Par(exprs = Seq(GString(string))), List(GInt(from), GInt(to)))
            val factor = to.toDouble / refSlice._2
            testProportional(baseCost, factor, method)
        }
      }
    }

    "called on byte arrays" should {
      "charge proportionally to the number of elements it has to traverse" in {
        val arrays = Table[GByteArray, Long, Long](
          ("list", "from", "to"),
          (gbyteArray(0), 0L, 0L),
          (gbyteArray(1), 0L, 1L),
          (gbyteArray(1000), 10L, 20L),
          (GByteArray(toRholangMap(mapN(10, GInt(10L))).toByteString), 0L, 20L)
        )
        val refSlice = (0, 1)
        val baseCost = sliceCost(refSlice._2)
        forAll(arrays) {
          case (array, from, to) =>
            val method = methodCall("slice", Par(exprs = Seq(array)), List(GInt(from), GInt(to)))
            val factor = to.toDouble / refSlice._2
            testProportional(baseCost, factor, method)
        }
      }
    }
  }

  "append" when {
    "called on List" should {
      "charge proportionally to length of argument List" in {
        val lists = Table(
          ("left", "right"),
          (emptyList, emptyList),
          (listN(1), listN(1)),
          (listN(1), listN(100)),
          (listN(100), listN(1)),
          (listN(100), listN(1000))
        )
        val refList = listN(1)
        val refCost = listAppendCost(refList)
        forAll(lists) {
          case (left, right) =>
            val method = EPlusPlus(toRholangList(left), toRholangList(right))
            val factor = right.size.toDouble / refList.size
            testProportional(refCost, factor, method)
        }
      }
    }

    "called on GByteArray" should {
      "charge proportionally to the logarithm of length of target byte array" in {
        val arrays = Table(
          ("left", "right"),
          (gbyteArray(1), gbyteArray(1)),
          (gbyteArray(1), gbyteArray(100)),
          (gbyteArray(100), gbyteArray(1)),
          (gbyteArray(100), gbyteArray(1000))
        )
        val refByteArray = gbyteArray(10)
        val refCost      = byteArrayAppendCost(refByteArray.value)
        forAll(arrays) {
          case (left, right) =>
            val method = EPlusPlus(left, right)
            val factor = {
              val f = math.log10(left.value.size.toDouble) / math.log10(
                refByteArray.value.size.toDouble
              )
              if (f === Double.NegativeInfinity || f === Double.PositiveInfinity)
                0
              else f
            }
            testProportional(refCost, factor, method)
        }
      }
    }

    "called on String" should {
      "charge proportionally to the sum of lengths of both Strings" in {
        val strings = Table(
          ("left", "right"),
          ("", ""),
          ("a", ""),
          ("", "a"),
          (stringN(100), "a"),
          ("a", stringN(100)),
          (stringN(1000), stringN(20))
        )
        val refPair                                = ("" -> "a")
        val refPairLength: (String, String) => Int = (l, r) => l.length + r.length
        val refCost                                = stringAppendCost(refPair._1.length, refPair._2.length)
        forAll(strings) {
          case (left, right) =>
            val method = EPlusPlus(GString(left), GString(right))
            val factor = (left.length + right.length) / refPairLength(refPair._1, refPair._2).toDouble
            testProportional(refCost, factor, method)
        }
      }
    }

    "called on Map" should {
      "charge proportionally to the size of the argument Maps" in {
        val maps = Table(
          ("left", "right"),
          (emptyMap, emptyMap),
          (emptyMap, map(Seq[(Par, Par)]((GInt(1), GString("one"))))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        val refArgMap = mapN(1, GString("1"))
        val refCost   = unionCost(refArgMap.size)
        forAll(maps) {
          case (left, right) =>
            val method = EPlusPlus(toRholangMap(left), toRholangMap(right))
            val factor = right.size.toDouble / refArgMap.size
            testProportional(refCost, factor, method)
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Sets" in {
        val sets = Table(
          ("left", "right"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        val refSet  = Set(Send(GString("result"), List(GString("Success")), false, BitSet()))
        val refCost = unionCost(refSet.size)
        forAll(sets) {
          case (left, right) =>
            val method = EPlusPlus(toRholangSet(left), toRholangSet(right))
            val factor = right.size.toDouble / refSet.size
            testProportional(refCost, factor, method)
        }
      }
    }

  }

  "interpolate" when {
    "called on String with a Map of values to interpolate with" should {
      "charge proportionally to the length of base String and size of the Map" in {
        def strMapN(n: Long): Map[Par, Par] =
          (1L to n).map(i => (GString(s"key$i"): Par, GInt(i): Par)).toMap

        val data = Table(
          ("string", "map"),
          ("a", strMapN(1)),
          (stringN(100), emptyMap),
          (stringN(100), strMapN(10))
        )
        def product(str: String, map: Map[Par, Par]): Double =
          str.length * map.size.toDouble

        val refPair    = ("a", strMapN(1))
        val refProduct = product(refPair._1, refPair._2)
        val refCost    = interpolateCost(refPair._1.length, refPair._2.size)
        forAll(data) {
          case (string, map) =>
            val method = EPercentPercent(GString(string), toRholangMap(map))
            val factor = {
              val f = product(string, map) / refProduct
              if (f == Double.PositiveInfinity) 0
              else f
            }
            testProportional(refCost, factor, method)
        }
      }
    }
  }

  "reducer" when {
    "it works with BigInt" should {
      val table = Table(
        ("left", "right"),
        (gBigInt("225"), gBigInt("25")),
        (
          gBigInt("9999999999999999999999999999999999999999"),
          gBigInt("-9999999999999999999999999999999999999999")
        ),
        (
          gBigInt("0"),
          gBigInt("9999999999999999999999999999999999999999")
        ),
        (
          gBigInt("123"),
          gBigInt("-9999999999999999999999999999999999999999")
        )
      )

      "charge expression `-x` as `size(x)`" in {
        forAll(table) {
          case (left, _) =>
            val expr           = ENegBody(ENeg(left))
            val expected: Long = size(left)
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 + x2` as `max(size(x1), size(x2)) + 1`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EPlusBody(EPlus(left, right))
            val expected: Long = scala.math.max(size(left), size(right)) + 1L
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 - x2` as `max(size(x1), size(x2)) + 1`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EMinusBody(EMinus(left, right))
            val expected: Long = scala.math.max(size(left), size(right)) + 1L
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 * x2` as `size(x1) * size(x2)`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EMultBody(EMult(left, right))
            val expected: Long = size(left) * size(right)
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 / x2` as `size(x1) * size(x2)`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EDivBody(EDiv(left, right))
            val expected: Long = size(left) * size(right)
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 % x2` as `size(x1) * size(x2)`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EModBody(EMod(left, right))
            val expected: Long = size(left) * size(right)
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 < x2` as `min(size(x1), size(x2))`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = ELtBody(ELt(left, right))
            val expected: Long = scala.math.min(size(left), size(right))
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 <= x2` as `min(size(x1), size(x2))`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = ELteBody(ELte(left, right))
            val expected: Long = scala.math.min(size(left), size(right))
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 > x2` as `min(size(x1), size(x2))`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EGtBody(EGt(left, right))
            val expected: Long = scala.math.min(size(left), size(right))
            test(expr, Cost(expected))
        }
      }
      "charge expression `x1 >= x2` as `min(size(x1), size(x2))`" in {
        forAll(table) {
          case (left, right) =>
            val expr           = EGteBody(EGte(left, right))
            val expected: Long = scala.math.min(size(left), size(right))
            test(expr, Cost(expected))
        }
      }
    }
  }

  "toInt() method" when {
    "working with Int value" should {
      "not require additional costs" in {
        val table = Table(
          "par",
          GInt(123),
          GInt(0),
          GInt(-99999)
        )
        forAll(table) { i =>
          val method         = methodCall("toInt", i, List())
          val expected: Long = 0
          test(method, Cost(expected))
        }
      }
    }
    "working with BigInt value" should {
      "charge `x.toInt()` as `size(x)`" in {
        val table = Table(
          "par",
          gBigInt("123"),
          gBigInt("0"),
          gBigInt("-99999")
        )
        forAll(table) { bi =>
          val method         = methodCall("toInt", bi, List())
          val expected: Long = size(bi)
          test(method, Cost(expected))
        }
      }
    }
    "working with BigInt value" should {
      "charge `x.toInt()` as `length(x)`" in {
        val table = Table(
          "par",
          GString("123"),
          GString("0"),
          GString("-99999")
        )
        forAll(table) { str =>
          val method         = methodCall("toInt", str, List())
          val expected: Long = str.value.length.toLong
          test(method, Cost(expected))
        }
      }
    }
  }

  "toBigInt method" when {
    "working with BigInt value" should {
      "doesn't have additional cost" in {
        val table = Table(
          "par",
          gBigInt("123"),
          gBigInt("0"),
          gBigInt("-9999999999999999999999999999999999999999")
        )
        forAll(table) { bi =>
          val method         = methodCall("toBigInt", bi, List())
          val expected: Long = 0
          test(method, Cost(expected))
        }
      }
    }
    "working with Int value" should {
      "charge `x.toInt()` as `8`" in {
        val table = Table(
          "par",
          GInt(123),
          GInt(0),
          GInt(-99999)
        )
        forAll(table) { i =>
          val method         = methodCall("toBigInt", i, List())
          val expected: Long = 8L
          test(method, Cost(expected))
        }
      }
    }
    "working with String value" should {
      "charge `x.toInt()` as `length(x)`" in {
        val table = Table(
          "par",
          GString("123"),
          GString("0"),
          GString("-9999999999999999999999999999999999999999")
        )
        forAll(table) { bi =>
          val method         = methodCall("toBigInt", bi, List())
          val expected: Long = bi.value.length.toLong
          test(method, Cost(expected))
        }
      }
    }
  }

  def methodCall(method: String, target: Par, arguments: List[Par]): Expr =
    EMethod(method, target, arguments)

  def methodCallCost(reducer: Reduce[IO])(implicit cost: _cost[IO]): IO[Cost] =
    cost.get
      .map(balance => Cost.UNSAFE_MAX - balance - METHOD_CALL_COST)

  def exprCallCost(reducer: Reduce[IO])(implicit cost: _cost[IO]): IO[Cost] =
    cost.get
      .map(balance => Cost.UNSAFE_MAX - balance)

  def map(pairs: Seq[(Par, Par)]): Map[Par, Par] = Map(pairs: _*)
  def emptyMap: Map[Par, Par]                    = map(Seq.empty[(Par, Par)])
  def mapN(n: Long, value: Par): Map[Par, Par] =
    (1L to n).map(i => (Par().withExprs(Seq(GInt(i))), value)).toMap
  def toRholangMap(map: Map[Par, Par]): EMapBody =
    EMapBody(ParMap(SortedParMap(map)))

  def emptySet: Set[Par] = setN(0)
  def setN(n: Long): Set[Par] =
    (1L to n).map(i => Par().withExprs(Seq(GInt(i)))).toSet
  def toRholangSet(set: Set[Par]): ESetBody =
    ESetBody(ParSet(set.toSeq))

  def listN(n: Long): Vector[Par] =
    (1L to n).map(i => Par().withExprs(Vector(GInt(i)))).toVector
  def emptyList: Vector[Par] = Vector.empty[Par]

  def toRholangList(vector: Vector[Par]): EListBody =
    EListBody(EList(vector))

  def gbyteArray(n: Int): GByteArray =
    GByteArray(ByteString.copyFrom(new Array[Byte](n)))

  def gBigInt(data: String): GBigInt = GBigInt(BigInt(data))

  def size(bi: GBigInt): Long = bi.value.toByteArray.length.toLong

  def stringN(n: Int): String =
    Seq.fill(n)("a").mkString

  def emptyString: String = ""

  def test(expr: Expr, expectedCost: Cost): Assertion = {
    implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync
    implicit val env  = Env[Par]()
    withReducer[Assertion] { reducer =>
      for {
        _ <- reducer.evalExprToPar(expr)
        cost <- expr.exprInstance match {
                 case EMethodBody(_) => methodCallCost(reducer)
                 case _              => exprCallCost(reducer)
               }

      } yield assert(cost.value === expectedCost.value)
    }
  }

  def withReducer[R](
      f: DebruijnInterpreter[IO] => IO[R]
  )(implicit cost: _cost[IO]): R = {

    val test = for {
      _   <- cost.set(Cost.UNSAFE_MAX)
      res <- f(RholangOnlyDispatcher(space)._2)
    } yield res
    test.unsafeRunSync
  }

  private var dbDir: Path              = null
  private var space: RhoTuplespace[IO] = null

  implicit val logF: Log[IO]            = new Log.NOPLog[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()
  implicit val ms: Metrics.Source       = Metrics.BaseSource
  implicit val kvm                      = InMemoryStoreManager[IO]
  val rSpaceStore                       = kvm.rSpaceStores.unsafeRunSync
  import coop.rchain.shared.RChainScheduler._

  protected override def beforeAll(): Unit = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: Match[IO, BindPattern, ListParWithRandom] = matchListPar[IO]
    dbDir = Files.createTempDirectory("rholang-interpreter-test-")
    space = RSpace
      .create[IO, Par, BindPattern, ListParWithRandom, TaggedContinuation](rSpaceStore, rholangEC)
      .unsafeRunSync
  }

  protected override def afterAll(): Unit = {
    import coop.rchain.shared.PathOps._
    dbDir.recursivelyDelete()
  }

}
