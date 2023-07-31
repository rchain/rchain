package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.models.rholangN.GBoolN
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoolMatcherSpec extends AnyFlatSpec with Matchers {
  "BoolTrue" should "Compile as GBool(true)" in {
    val btrue = new BoolTrue()

    BoolNormalizeMatcher.normalizeMatch(btrue) should be(GBoolN(true))
  }
  "BoolFalse" should "Compile as GBool(false)" in {
    val bfalse = new BoolFalse()

    BoolNormalizeMatcher.normalizeMatch(bfalse) should be(GBoolN(false))
  }
}
