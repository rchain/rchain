package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_Nil_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [NIL, EOF] for \"Nil\"" in {
    val tokens = tokenize("Nil", collector)

    tokens shouldBe asList(NIL.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Nil_\"" in {
    val tokens = tokenize("Nil_", collector)

    tokens shouldBe asList(IDENT.T("Nil_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"NilX\"" in {
    val tokens = tokenize("NilX", collector)

    tokens shouldBe asList(IDENT.T("NilX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"NilNil\"" in {
    val tokens = tokenize("NilNil", collector)

    tokens shouldBe asList(IDENT.T("NilNil"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Nil0\"" in {
    val tokens = tokenize("Nil0", collector)

    tokens shouldBe asList(IDENT.T("Nil0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NIL, PLUS, EOF] for \"Nil+\"" in {
    val tokens = tokenize("Nil+", collector)

    tokens shouldBe asList(NIL.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NIL, ERROR, EOF] for \"Nil$\"" in {
    val tokens = tokenize("Nil$", collector)

    tokens shouldBe asList(NIL.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
