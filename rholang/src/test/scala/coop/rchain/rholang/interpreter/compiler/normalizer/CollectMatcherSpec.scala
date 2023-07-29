package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.Eval
import coop.rchain.catscontrib.effect.implicits.sEval
import coop.rchain.models._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.ParBuilderUtil
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.errors._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectMatcherSpec extends AnyFlatSpec with Matchers {
  val inputs = ProcVisitInputs(
    NilN(),
    BoundMapChain
      .empty[VarSort]
      .put(List(("P", ProcSort, SourcePosition(0, 0)), ("x", NameSort, SourcePosition(0, 0)))),
    FreeMap.empty[VarSort]
  )
  implicit val normalizerEnv: Map[String, Par] = Map.empty
  def getNormalizedPar(rho: String): Par       = ParBuilderUtil.mkTerm(rho).toOption.get
  def assertEqualNormalized(rho1: String, rho2: String): Assertion =
    assert(getNormalizedPar(rho1) == getNormalizedPar(rho2))

  "List" should "delegate" in {
    val listData = new ListProc()
    listData.add(new PVar(new ProcVarVar("P")))
    listData.add(new PEval(new NameVar("x")))
    listData.add(new PGround(new GroundInt("7")))
    val list = new PCollect(new CollectList(listData, new ProcRemainderEmpty()))

    val result = ProcNormalizeMatcher.normalizeMatch[Eval](list, inputs).value
    result.par should be(EListN(Seq(BoundVarN(1), BoundVarN(0), GIntN(7))))
    result.freeMap should be(inputs.freeMap)
  }
  "List" should "sort the insides of their elements" in {
    assertEqualNormalized("@0!([{1 | 2}])", "@0!([{2 | 1}])")
  }
  "List" should "sort the insides of a send encoded as a byte array" in {
    val rho1 =
      """new x in {
        |  x!(
        |    [
        |      @"a"!(
        |        @"x"!("abc") |
        |        @"y"!(1)
        |      )
        |    ].toByteArray()
        |  )}""".stripMargin
    val rho2 =
      """new x in {
        |  x!(
        |    [
        |      @"a"!(
        |        @"y"!(1) |
        |        @"x"!("abc")
        |      )
        |    ].toByteArray()
        |  )}""".stripMargin
    assertEqualNormalized(rho1, rho2)
  }

  "Tuple" should "delegate" in {
    val tupleData = new ListProc()
    tupleData.add(new PEval(new NameVar("y")))
    val tuple =
      new PCollect(new CollectTuple(new TupleMultiple(new PVar(new ProcVarVar("Q")), tupleData)))

    val result = ProcNormalizeMatcher.normalizeMatch[Eval](tuple, inputs).value
    result.par should be(ETupleN(Seq(FreeVarN(0), FreeVarN(1))))
    result.freeMap should be(
      inputs.freeMap.put(
        List(("Q", ProcSort, SourcePosition(0, 0)), ("y", NameSort, SourcePosition(0, 0)))
      )
    )
  }
  "Tuple" should "propagate free variables" in {
    val tupleData = new ListProc()
    tupleData.add(new PGround(new GroundInt("7")))
    tupleData.add(new PPar(new PGround(new GroundInt("7")), new PVar(new ProcVarVar("Q"))))
    val tuple =
      new PCollect(new CollectTuple(new TupleMultiple(new PVar(new ProcVarVar("Q")), tupleData)))

    an[UnexpectedReuseOfProcContextFree] should be thrownBy {
      ProcNormalizeMatcher.normalizeMatch[Eval](tuple, inputs).value
    }
  }
  "Tuple" should "sort the insides of their elements" in {
    assertEqualNormalized("@0!(({1 | 2}))", "@0!(({2 | 1}))")
  }

  "Set" should "delegate" in {
    val setData = new ListProc()
    setData.add(new PAdd(new PVar(new ProcVarVar("P")), new PVar(new ProcVarVar("R"))))
    setData.add(new PGround(new GroundInt("7")))
    setData.add(new PPar(new PGround(new GroundInt("8")), new PVar(new ProcVarVar("Q"))))
    val set = new PCollect(new CollectSet(setData, new ProcRemainderVar(new ProcVarVar("Z"))))

    val result = ProcNormalizeMatcher.normalizeMatch[Eval](set, inputs).value

    result.par should be(
      ESetN(
        Seq(EPlusN(BoundVarN(1), FreeVarN(1)), GIntN(7), GIntN(8).add(FreeVarN(2))),
        Some(FreeVarN(0))
      )
    )
    val newBindings = List(
      ("Z", ProcSort, SourcePosition(0, 0)),
      ("R", ProcSort, SourcePosition(0, 0)),
      ("Q", ProcSort, SourcePosition(0, 0))
    )
    result.freeMap should be(inputs.freeMap.put(newBindings))
  }
  "Set" should "sort the insides of their elements" in {
    assertEqualNormalized("@0!(Set({1 | 2}))", "@0!(Set({2 | 1}))")
  }
  "Map" should "delegate" in {
    val mapData = new ListKeyValuePair()
    mapData.add(
      new KeyValuePairImpl(
        new PGround(new GroundInt("7")),
        new PGround(new GroundString("\"Seven\""))
      )
    )
    mapData.add(new KeyValuePairImpl(new PVar(new ProcVarVar("P")), new PEval(new NameVar("Q"))))
    val map = new PCollect(new CollectMap(mapData, new ProcRemainderVar(new ProcVarVar("Z"))))

    val result = ProcNormalizeMatcher.normalizeMatch[Eval](map, inputs).value
    result.par should be(
      EMapN(Seq(GIntN(7) -> GStringN("Seven"), BoundVarN(1) -> FreeVarN(1)), Some(FreeVarN(0)))
    )
    val newBindings = List(
      ("Z", ProcSort, SourcePosition(0, 0)),
      ("Q", NameSort, SourcePosition(0, 0))
    )
    result.freeMap should be(inputs.freeMap.put(newBindings))
  }
  "Map" should "sort the insides of their keys" in {
    assertEqualNormalized("@0!({{1 | 2} : 0})", "@0!({{2 | 1} : 0})")
  }
  "Map" should "sort the insides of their values" in {
    assertEqualNormalized("@0!({0 : {1 | 2}})", "@0!({0 : {2 | 1}})")
  }
}
