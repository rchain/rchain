package coop.rchain.rholang.interpreter.accounting
import java.nio.file.{Files, Path}

import cats.Id
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter._
import org.scalatest.{Assertion, BeforeAndAfterAll, WordSpec}
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
import org.scalatest.prop.TableFor1

import scala.collection.immutable.BitSet

class RholangMethodsCostsSpec extends WordSpec with TripleEqualsSupport with BeforeAndAfterAll {

  "nth method" when {
    "called on a list" should {
      "charge constant cost regardless of collection size or value of n" in {
        implicit val errorLog = new ErrorLog()
        val table = Table(
          ("list", "index"),
          (List[Par](GInt(1)), 0),
          (List[Par](GInt(1), GInt(2)), 1),
          (Seq.fill[Par](10000)(GInt(1)).toList, 1000)
        )
        forAll(table) {
          case (list, arg) =>
            test(methodCall("nth", EList(list), List(GInt(arg))), NTH_METHOD_CALL_COST)
        }
      }
    }
  }

  "toByteArray" when {
    "called on rholang term" should {
      "charge proportionally to the byte size of the underlying term" in {
        implicit val errorLog = new ErrorLog()
        val pars: TableFor1[Par] = Table(
          "par",
          Par(),
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
        forAll(pars) { par =>
          val substitutionCost = Cost(par)
          test(methodCall("toByteArray", par, List.empty), toByteArrayCost(par) + substitutionCost)
        }
      }

    }
  }

  "hexToBytes" when {
    "called on hex encoded string" should {
      "charge proportionally to the length of the string" in {
        implicit val errorLog = new ErrorLog()
        val strings = Table(
          "string",
          "",
          "abcdef",
          Seq.fill(1000)("a").mkString("")
        )
        forAll(strings) { str =>
          val encodedStr = new String(str.getBytes("UTF-8"))
          test(methodCall("hexToBytes", GString(encodedStr), List.empty), hexToByteCost(encodedStr))
        }
      }
    }
  }

  "union" when {
    "called on Map" should {
      "charge proportionally to the size of the argument Map" in {
        implicit val errorLog = new ErrorLog()
        val maps = Table(
          ("baseMap", "argumentMap"),
          (emptyMap, emptyMap),
          (emptyMap, map(Seq[(Par, Par)]((GInt(1), GString("one"))))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        forAll(maps) {
          case (baseMap, argMap) =>
            test(
              methodCall("union", toRholangMap(baseMap), List(toRholangMap(argMap))),
              unionCost(argMap.size)
            )
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Set" in {
        implicit val errorLog = new ErrorLog()
        val sets = Table(
          ("baseSet", "argumentSet"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        forAll(sets) {
          case (baseSet, argSet) =>
            test(
              methodCall("union", toRholangSet(baseSet), List(toRholangSet(argSet))),
              unionCost(argSet.size)
            )
        }
      }
    }
  }

  "diff" when {
    "called on Map" should {
      "charge proportionally to the size of the argument Map" in {
        implicit val errorLog = new ErrorLog()
        val maps = Table(
          ("baseMap", "argumentMap"),
          (emptyMap, emptyMap),
          (emptyMap, map(Seq[(Par, Par)]((GInt(1), GString("one"))))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        forAll(maps) {
          case (baseMap, argMap) =>
            test(
              methodCall("diff", toRholangMap(baseMap), List(toRholangMap(argMap))),
              diffCost(argMap.size)
            )
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Set" in {
        implicit val errorLog = new ErrorLog()
        val maps = Table(
          ("baseSet", "argumentSet"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        forAll(maps) {
          case (baseSet, argSet) =>
            test(
              methodCall("diff", toRholangSet(baseSet), List(toRholangSet(argSet))),
              diffCost(argSet.size)
            )
        }
      }
    }
  }

  "add" when {
    "called on Set" should {
      "charge constant cost" in {
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
        val maps = Table[Map[Par, Par]](
          "map",
          emptyMap,
          mapN(1, GInt(1)),
          mapN(100, GInt(1)),
          map(Seq((toRholangMap(mapN(10, GInt(10))), GInt(1))))
        )
        forAll(maps) { map =>
          test(methodCall("size", toRholangMap(map), List()), sizeMethodCost(map.size))
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the number of elements in the base Set" in {
        implicit val errorLog = new ErrorLog()
        val sets = Table[Set[Par]](
          "set",
          emptySet,
          Set(GInt(1)),
          setN(100),
          Set(toRholangMap(mapN(10, GInt(10))))
        )
        forAll(sets) { set =>
          test(methodCall("size", toRholangSet(set), List()), sizeMethodCost(set.size))
        }
      }
    }
  }

  "length" when {
    "called on List" should {
      "charge constant cost" in {
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
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
        implicit val errorLog = new ErrorLog()
        val lists = Table[Vector[Par], Int, Int](
          ("list", "from", "to"),
          (Vector.empty[Par], 0, 1),
          (Vector.empty[Par], 1, 0),
          (Vector[Par](GInt(1)), 0, 1),
          (setN(100).toVector, 10, 20),
          (Vector[Par](toRholangMap(mapN(10, GInt(10)))), 0, 1)
        )
        forAll(lists) {
          case (list, from, to) =>
            test(
              methodCall("slice", toRholangList(list), List(GInt(from), GInt(to))),
              sliceCost(to)
            )
        }
      }
    }

    "called on String" should {
      "charge proportionally to the number of elements it has to traverse" in {
        implicit val errorLog = new ErrorLog()
        val strings = Table[String, Int, Int, Int](
          ("string", "from", "to", "cost"),
          ("", 0, 0, 0),
          ("", 1, 0, 0),
          ("abcd", 2, 4, 4),
          (Seq.fill(10000)("").mkString, 10, 100, 90)
        )
        forAll(strings) {
          case (string, from, to, cost) =>
            test(
              methodCall("slice", Par(exprs = Seq(GString(string))), List(GInt(from), GInt(to))),
              sliceCost(to)
            )
        }
      }
    }

    "called on byte arrays" should {
      "charge proportionally to the number of elements it has to traverse" in {
        implicit val errorLog = new ErrorLog()
        val arrays = Table[GByteArray, Int, Int](
          ("list", "from", "to"),
          (gbyteArray(0), 0, 0),
          (gbyteArray(1), 0, 1),
          (gbyteArray(1000), 10, 20),
          (GByteArray(toRholangMap(mapN(10, GInt(10))).toByteString), 0, 20)
        )
        forAll(arrays) {
          case (array, from, to) =>
            test(
              methodCall("slice", Par(exprs = Seq(array)), List(GInt(from), GInt(to))),
              sliceCost(to)
            )
        }
      }
    }
  }

  "append" when {
    "called on List" should {
      "charge proportionally to the sum of lengths of both Lists" in {
        implicit var errorLog = new ErrorLog()
        val lists = Table(
          ("left", "right"),
          (emptyList, emptyList),
          (listN(1), listN(1)),
          (listN(100), listN(1000))
        )
        forAll(lists) {
          case (left, right) =>
            test(
              EPlusPlus(toRholangList(left), toRholangList(right)),
              appendCost(left.length, right.length)
            )
        }
      }
    }

    "called on GByteArray" should {
      "charge proportionally to the sum of lengths of both Arrays" in {
        implicit var errorLog = new ErrorLog()
        val arrays = Table(
          ("left", "right"),
          (gbyteArray(0), gbyteArray(0)),
          (gbyteArray(1), gbyteArray(1)),
          (gbyteArray(100), gbyteArray(1000))
        )
        forAll(arrays) {
          case (left, right) =>
            test(EPlusPlus(left, right), appendCost(left.value.size(), right.value.size()))
        }
      }
    }

    "called on String" should {
      "charge proportionally to the sum of lengths of both Strings" in {
        implicit var errorLog = new ErrorLog()
        val strings = Table(
          ("left", "right"),
          ("", ""),
          ("a", ""),
          ("", "a"),
          (stringN(100), "a"),
          ("a", stringN(100)),
          (stringN(1000), stringN(20))
        )
        forAll(strings) {
          case (left, right) =>
            test(EPlusPlus(GString(left), GString(right)), appendCost(left.length, right.length))
        }
      }
    }

    "called on Map" should {
      "charge proportionally to the size of the argument Maps" in {
        implicit val errorLog = new ErrorLog()
        val maps = Table(
          ("left", "right"),
          (emptyMap, emptyMap),
          (emptyMap, map(Seq[(Par, Par)]((GInt(1), GString("one"))))),
          (mapN(1, GString("one")), emptyMap),
          (mapN(1, GString("one")), mapN(1, GString("two"))),
          (mapN(10, GString("one")), mapN(1000, GString("two"))),
          (mapN(1000, GString("one")), mapN(1, GString("two")))
        )
        forAll(maps) {
          case (left, right) =>
            test(EPlusPlus(toRholangMap(left), toRholangMap(right)), unionCost(right.size))
        }
      }
    }

    "called on Set" should {
      "charge proportionally to the size of the argument Sets" in {
        implicit val errorLog = new ErrorLog()
        val sets = Table(
          ("left", "right"),
          (emptySet, emptySet),
          (emptySet, setN(1)),
          (setN(1), emptySet),
          (setN(1), setN(1)),
          (setN(1), setN(1000)),
          (setN(1000), setN(1))
        )
        forAll(sets) {
          case (left, right) =>
            test(EPlusPlus(toRholangSet(left), toRholangSet(right)), unionCost(right.size))
        }
      }
    }

  }

  "interpolate" when {
    "called on String with a Map of values to interpolate with" should {
      "charge proportionally to the length of base String and size of the Map" in {
        implicit val errorLog = new ErrorLog()
        def strMapN(n: Int): Map[Par, Par] =
          (1 to n).map(i => (GString(s"key$i"): Par, GInt(i): Par)).toMap

        val data = Table(
          ("string", "map"),
          ("", emptyMap),
          (stringN(100), emptyMap),
          (stringN(100), strMapN(10))
        )
        forAll(data) {
          case (string, map) =>
            test(
              EPercentPercent(GString(string), toRholangMap(map)),
              interpolateCost(string.length, map.size)
            )
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
  def mapN(n: Int, value: Par): Map[Par, Par] =
    (1 to n).map(i => (Par().withExprs(Seq(GInt(i))), value)).toMap
  def toRholangMap(map: Map[Par, Par]): EMapBody =
    EMapBody(ParMap(SortedParMap(map)))

  def emptySet: Set[Par] = setN(0)
  def setN(n: Int): Set[Par] =
    (1 to n).map(i => Par().withExprs(Seq(GInt(i)))).toSet
  def toRholangSet(set: Set[Par]): ESetBody =
    ESetBody(ParSet(set.toSeq))

  def listN(n: Int): Vector[Par] =
    (1 to n).map(i => Par().withExprs(Vector(GInt(i)))).toVector
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
    test.runSyncUnsafe(3.seconds)
  }

  private var dbDir: Path         = null
  private var context: RhoContext = null
  private var space: RhoISpace    = null

  override protected def beforeAll(): Unit = {
    import coop.rchain.rholang.interpreter.storage.implicits._
    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    dbDir = Files.createTempDirectory("rholang-interpreter-test-")
    context = Context.createInMemory()
    space = RSpace.create[
      Id,
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ](context, Branch("rholang-methods-cost-test"))
  }

  override protected def afterAll(): Unit = {
    import coop.rchain.shared.PathOps._
    space.close()
    context.close()
    dbDir.recursivelyDelete()
  }

}
