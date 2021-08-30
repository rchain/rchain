package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import org.scalatest._

import scala.collection.immutable.BitSet
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.ParBuilderUtil
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  IndexMapChain,
  NameSort,
  ProcSort,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition,
  VarSort
}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import monix.eval.Coeval

class CollectMatcherSpec extends FlatSpec with Matchers {
  val inputs = ProcVisitInputs(
    Par(),
    IndexMapChain
      .empty[VarSort]
      .put(List(("P", ProcSort, SourcePosition(0, 0)), ("x", NameSort, SourcePosition(0, 0)))),
    DeBruijnLevelMap.empty[VarSort]
  )
  implicit val normalizerEnv: Map[String, Par] = Map.empty
  def getNormalizedPar(rho: String): Par       = ParBuilderUtil.buildNormalizedTerm[Coeval](rho).value()
  def assertEqualNormalized(rho1: String, rho2: String): Assertion =
    assert(getNormalizedPar(rho1) == getNormalizedPar(rho2))

  "List" should "delegate" in {
    val listData = new ListProc()
    listData.add(new PVar(new ProcVarVar("P")))
    listData.add(new PEval(new NameVar("x")))
    listData.add(new PGround(new GroundInt("7")))
    val list = new PCollect(new CollectList(listData, new ProcRemainderEmpty()))

    val result =
      Normalizer[Coeval, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(list, inputs).value
    result.par should be(
      inputs.par.prepend(
        EList(
          List[Par](EVar(BoundVar(1)), EVar(BoundVar(0)), GInt(7)),
          locallyFree = BitSet(0, 1)
        ),
        0
      )
    )
    result.knownFree should be(inputs.knownFree)
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

    val result = Normalizer[Coeval, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
      .normalize(tuple, inputs)
      .value
    result.par should be(
      inputs.par.prepend(
        ETuple(
          List[Par](
            EVar(FreeVar(0)),
            EVar(FreeVar(1))
          ),
          locallyFree = BitSet(),
          connectiveUsed = true
        ),
        0
      )
    )
    result.knownFree should be(
      inputs.knownFree.put(
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
      Normalizer[Coeval, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
        .normalize(tuple, inputs)
        .value
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

    val result =
      Normalizer[Coeval, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(set, inputs).value

    result.par should be(
      inputs.par.prepend(
        ParSet(
          Seq[Par](
            EPlus(EVar(BoundVar(1)), EVar(FreeVar(1))),
            GInt(7),
            GInt(8).prepend(EVar(FreeVar(2)), 0)
          ),
          remainder = Some(FreeVar(0))
        ),
        depth = 0
      )
    )
    val newBindings = List(
      ("Z", ProcSort, SourcePosition(0, 0)),
      ("R", ProcSort, SourcePosition(0, 0)),
      ("Q", ProcSort, SourcePosition(0, 0))
    )
    result.knownFree should be(inputs.knownFree.put(newBindings))
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

    val result =
      Normalizer[Coeval, Proc, ProcVisitInputs, ProcVisitOutputs, Par].normalize(map, inputs).value
    result.par should be(
      inputs.par.prepend(
        ParMap(
          List[(Par, Par)](
            (GInt(7), GString("Seven")),
            (EVar(BoundVar(1)), EVar(FreeVar(1)))
          ),
          locallyFree = BitSet(1),
          connectiveUsed = true,
          remainder = Some(Var(FreeVar(0)))
        ),
        depth = 0
      )
    )
    val newBindings = List(
      ("Z", ProcSort, SourcePosition(0, 0)),
      ("Q", NameSort, SourcePosition(0, 0))
    )
    result.knownFree should be(inputs.knownFree.put(newBindings))
  }
  "Map" should "sort the insides of their keys" in {
    assertEqualNormalized("@0!({{1 | 2} : 0})", "@0!({{2 | 1} : 0})")
  }
  "Map" should "sort the insides of their values" in {
    assertEqualNormalized("@0!({0 : {1 | 2}})", "@0!({0 : {2 | 1}})")
  }
}
