package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_else_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ELSE, EOF] for \"else\"" in {
    val tokens = tokenize("else", collector)

    tokens shouldBe asList(ELSE.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"else_\"" in {
    val tokens = tokenize("else_", collector)

    tokens shouldBe asList(IDENT.T("else_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"elseX\"" in {
    val tokens = tokenize("elseX", collector)

    tokens shouldBe asList(IDENT.T("elseX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"elseelse\"" in {
    val tokens = tokenize("elseelse", collector)

    tokens shouldBe asList(IDENT.T("elseelse"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"else0\"" in {
    val tokens = tokenize("else0", collector)

    tokens shouldBe asList(IDENT.T("else0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ELSE, PLUS, EOF] for \"else+\"" in {
    val tokens = tokenize("else+", collector)

    tokens shouldBe asList(ELSE.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ELSE, ERROR, EOF] for \"else$\"" in {
    val tokens = tokenize("else$", collector)

    tokens shouldBe asList(ELSE.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
