package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import org.scalatest._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.rholang.interpreter.normalizer.instances.Proc.normalizeBool

class BoolMatcherSpec extends FlatSpec with Matchers {
  "BoolTrue" should "Compile as GBool(true)" in {
    val btrue = new BoolTrue()

    normalizeBool(btrue) should be(GBool(true))
  }
  "BoolFalse" should "Compile as GBool(false)" in {
    val bfalse = new BoolFalse()

    normalizeBool(bfalse) should be(GBool(false))
  }
}
