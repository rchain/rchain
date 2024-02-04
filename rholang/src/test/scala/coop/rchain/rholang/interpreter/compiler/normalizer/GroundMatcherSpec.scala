package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.Eval
import coop.rchain.catscontrib.effect.implicits.sEval
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GroundMatcherSpec extends AnyFlatSpec with Matchers {
  "GroundInt" should "Compile as GInt" in {
    val gi             = new GroundInt("7")
    val expectedResult = GIntN(7)
    GroundNormalizeMatcher.normalizeMatch[Eval](gi).value should be(expectedResult)
  }
  "Positive groundBigInt" should "Compile GBigInt" in {
    val gbi            = new GroundBigInt("9999999999999999999999999999999999999999")
    val expectedResult = GBigIntN(BigInt("9999999999999999999999999999999999999999"))
    GroundNormalizeMatcher.normalizeMatch[Eval](gbi).value should be(expectedResult)
  }
  "GroundString" should "Compile as GString" in {
    val gs             = new GroundString("\"String\"")
    val expectedResult = GStringN("String")
    GroundNormalizeMatcher.normalizeMatch[Eval](gs).value should be(expectedResult)
  }
  "GroundUri" should "Compile as GUri" in {
    val gu             = new GroundUri("`rho:uri`")
    val expectedResult = GUriN("rho:uri")
    GroundNormalizeMatcher.normalizeMatch[Eval](gu).value should be(expectedResult)
  }
}
