package coop.rchain.rholang.interpreter.accounting

import java.nio.file.{Files, Path}

import cats.Id
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter._
import org.scalatest.{Assertion, BeforeAndAfterAll, Matchers, WordSpec}
import coop.rchain.models.rholang.implicits._
import monix.eval.Task
import org.scalatest.prop.PropertyChecks._

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import coop.rchain.models.testImplicits._
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace}
import coop.rchain.rholang.interpreter.accounting.Chargeable._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Context, ISpace, RSpace}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.TableFor1

import scala.collection.immutable.BitSet

class RholangMethodsCostsSpec
    extends WordSpec
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

      "not charge when index is out of bound" in {
        val table = Table(
          ("list", "index"),
          (listN(0), 1L)
        )
        forAll(table) { (pars, n) =>
          implicit val errLog = new ErrorLog()
          implicit val env    = Env[Par]()
          val method          = methodCall("nth", EList(pars), List(GInt(n)))
          withReducer[Assertion] { reducer =>
            for {
              err  <- reducer.evalExprToPar(method).attempt
              _    = assert(err.isLeft)
              cost <- methodCallCost(reducer)
            } yield assert(cost === Cost(0))
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

      "not charge when index is out of bound" in {
        val table = Table(
          ("list", "index"),
          (listN(0), 1L)
        )
        forAll(table) { (pars, n) =>
          implicit val errLog = new ErrorLog()
          implicit val env    = Env[Par]()
          val method          = methodCall("nth", EList(pars), List(GInt(n)))
          withReducer[Assertion] { reducer =>
            for {
              err  <- reducer.evalExprToPar(method).attempt
              _    = assert(err.isLeft)
              cost <- methodCallCost(reducer)
            } yield assert(cost === Cost(0))
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
    implicit val errorLog = new ErrorLog()
    implicit val env      = Env[Par]()
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
          GString("Hello")
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

  def methodCall(method: String, target: Par, arguments: List[Par]): Expr =
    EMethod(method, target, arguments)

  def methodCallCost(reducer: ChargingReducer[Task]): Task[Cost] =
    reducer
      .getAvailablePhlos()
      .map(ca => Cost(Integer.MAX_VALUE) - ca.cost - METHOD_CALL_COST)

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

  def stringN(n: Int): String =
    Seq.fill(n)("a").mkString

  def emptyString: String = ""

  def test(method: Expr, expectedCost: Cost): Assertion = {
    implicit val errLog = new ErrorLog()
    implicit val env    = Env[Par]()
    withReducer[Assertion] { reducer =>
      for {
        _    <- reducer.evalExprToPar(method)
        cost <- methodCallCost(reducer)
      } yield assert(cost === expectedCost)
    }
  }
  def withReducer[R](f: ChargingReducer[Task] => Task[R])(implicit errLog: ErrorLog): R = {
    val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    val test = for {
      _   <- reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE))
      res <- f(reducer)
    } yield res
    test.runSyncUnsafe(5.seconds)
  }

  private var dbDir: Path            = null
  private var context: RhoContext    = null
  private var space: RhoISpace[Task] = null

  override protected def beforeAll(): Unit = {
    import coop.rchain.catscontrib.TaskContrib._
    import coop.rchain.rholang.interpreter.storage.implicits._
    dbDir = Files.createTempDirectory("rholang-interpreter-test-")
    context = Context.createInMemory()
    space = (
      RSpace
        .create[
          Task,
          Par,
          BindPattern,
          OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos,
          TaggedContinuation
        ](
          context,
          Branch("rholang-methods-cost-test")
        )
      )
      .unsafeRunSync
  }

  override protected def afterAll(): Unit = {
    import coop.rchain.shared.PathOps._
    space.close()
    context.close()
    dbDir.recursivelyDelete()
  }

}
