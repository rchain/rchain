package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_Bool_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [BOOL, EOF] for \"Bool\"" in {
    val tokens = tokenize("Bool", collector)

    tokens shouldBe asList(BOOL.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Bool_\"" in {
    val tokens = tokenize("Bool_", collector)

    tokens shouldBe asList(IDENT.T("Bool_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"BoolX\"" in {
    val tokens = tokenize("BoolX", collector)

    tokens shouldBe asList(IDENT.T("BoolX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"BoolBool\"" in {
    val tokens = tokenize("BoolBool", collector)

    tokens shouldBe asList(IDENT.T("BoolBool"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Bool0\"" in {
    val tokens = tokenize("Bool0", collector)

    tokens shouldBe asList(IDENT.T("Bool0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [BOOL, PLUS, EOF] for \"Bool+\"" in {
    val tokens = tokenize("Bool+", collector)

    tokens shouldBe asList(BOOL.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [BOOL, ERROR, EOF] for \"Bool$\"" in {
    val tokens = tokenize("Bool$", collector)

    tokens shouldBe asList(BOOL.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
