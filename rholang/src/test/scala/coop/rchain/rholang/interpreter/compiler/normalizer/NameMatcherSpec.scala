package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.rholang.syntax.rholang_mercury.Absyn._
import org.scalatest._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  IndexMapChain,
  NameSort,
  NameVisitInputs,
  ProcSort,
  SourcePosition,
  VarSort
}
import monix.eval.Coeval

class NameMatcherSpec extends FlatSpec with Matchers {
  val inputs                                   = NameVisitInputs(IndexMapChain.empty[VarSort], DeBruijnLevelMap.empty[VarSort])
  implicit val normalizerEnv: Map[String, Par] = Map.empty

  "NameWildcard" should "add a wildcard count to knownFree" in {
    val nw                  = new NameWildcard()
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nw, inputs).value
    val expectedResult: Par = EVar(Wildcard(Var.WildcardMsg()))
    result.chan should be(expectedResult)
    result.knownFree.count shouldEqual 1
  }

  val nvar = new NameVar("x")

  "NameVar" should "Compile as BoundVar if it's in env" in {
    val boundInputs = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))

    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nvar, boundInputs).value
    val expectedResult: Par = EVar(BoundVar(0))
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree)
  }
  "NameVar" should "Compile as FreeVar if it's not in env" in {
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nvar, inputs).value
    val expectedResult: Par = EVar(FreeVar(0))
    result.chan should be(expectedResult)
    result.knownFree shouldEqual
      (inputs.knownFree.put(("x", NameSort, SourcePosition(0, 0))))
  }
  "NameVar" should "Not compile if it's in env of the wrong sort" in {
    val boundInputs = inputs.copy(env = inputs.env.put(("x", ProcSort, SourcePosition(0, 0))))

    an[UnexpectedNameContext] should be thrownBy {
      NameNormalizeMatcher.normalizeMatch[Coeval](nvar, boundInputs).value
    }
  }
  "NameVar" should "Not compile if it's used free somewhere else" in {
    val boundInputs =
      inputs.copy(knownFree = inputs.knownFree.put(("x", NameSort, SourcePosition(0, 0))))

    an[UnexpectedReuseOfNameContextFree] should be thrownBy {
      NameNormalizeMatcher.normalizeMatch[Coeval](nvar, boundInputs).value
    }
  }

  val nqvar = new NameQuote(new PVar(new ProcVarVar("x")))

  "NameQuote" should "compile to a var if the var is bound" in {
    val boundInputs         = inputs.copy(env = inputs.env.put(("x", ProcSort, SourcePosition(0, 0))))
    val nqvar               = new NameQuote(new PVar(new ProcVarVar("x")))
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nqvar, boundInputs).value
    val expectedResult: Par = EVar(BoundVar(0))
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree)
  }

  "NameQuote" should "return a free use if the quoted proc has a free var" in {
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nqvar, inputs).value
    val expectedResult: Par = EVar(FreeVar(0))
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree.put(("x", ProcSort, SourcePosition(0, 0))))
  }

  "NameQuote" should "compile to a ground" in {
    val nqground            = new NameQuote(new PGround(new GroundInt("7")))
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nqground, inputs).value
    val expectedResult: Par = GInt(7)
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree)
  }

  "NameQuote" should "collapse an eval" in {
    val nqeval              = new NameQuote(new PEval(new NameVar("x")))
    val boundInputs         = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nqeval, boundInputs).value
    val expectedResult: Par = EVar(BoundVar(0))
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree)
  }

  "NameQuote" should "not collapse an eval | eval" in {
    val nqeval              = new NameQuote(new PPar(new PEval(new NameVar("x")), new PEval(new NameVar("x"))))
    val boundInputs         = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result              = NameNormalizeMatcher.normalizeMatch[Coeval](nqeval, boundInputs).value
    val expectedResult: Par = EVar(BoundVar(0)).prepend(EVar(BoundVar(0)), 0)
    result.chan should be(expectedResult)
    result.knownFree should be(inputs.knownFree)
  }

}
