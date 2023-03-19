package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.rholang.ast.rholang_mercury.Absyn._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import cats.Eval

class GroundMatcherSpec extends AnyFlatSpec with Matchers {
  "GroundInt" should "Compile as GInt" in {
    val gi                   = new GroundInt("7")
    val expectedResult: Expr = GInt(7)
    GroundNormalizeMatcher.normalizeMatch[Eval](gi).value should be(expectedResult)
  }
  "Positive groundBigInt" should "Compile GBigInt" in {
    val gbi                  = new GroundBigInt("9999999999999999999999999999999999999999")
    val expectedResult: Expr = GBigInt(BigInt("9999999999999999999999999999999999999999"))
    GroundNormalizeMatcher.normalizeMatch[Eval](gbi).value should be(expectedResult)
  }
  "GroundString" should "Compile as GString" in {
    val gs                   = new GroundString("\"String\"")
    val expectedResult: Expr = GString("String")
    GroundNormalizeMatcher.normalizeMatch[Eval](gs).value should be(expectedResult)
  }
  "GroundUri" should "Compile as GUri" in {
    val gu                   = new GroundUri("`rho:uri`")
    val expectedResult: Expr = GUri("rho:uri")
    GroundNormalizeMatcher.normalizeMatch[Eval](gu).value should be(expectedResult)
  }
}
