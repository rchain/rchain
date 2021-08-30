package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import org.scalatest._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.normalizer.instances.Proc.normalizeGround
import monix.eval.Coeval

class GroundMatcherSpec extends FlatSpec with Matchers {
  "GroundInt" should "Compile as GInt" in {
    val gi                   = new GroundInt("7")
    val expectedResult: Expr = GInt(7)
    normalizeGround[Coeval](gi).value should be(expectedResult)
  }
  "GroundString" should "Compile as GString" in {
    val gs                   = new GroundString("\"String\"")
    val expectedResult: Expr = GString("String")
    normalizeGround[Coeval](gs).value should be(expectedResult)
  }
  "GroundUri" should "Compile as GUri" in {
    val gu                   = new GroundUri("`rho:uri`")
    val expectedResult: Expr = GUri("rho:uri")
    normalizeGround[Coeval](gu).value should be(expectedResult)
  }
}
