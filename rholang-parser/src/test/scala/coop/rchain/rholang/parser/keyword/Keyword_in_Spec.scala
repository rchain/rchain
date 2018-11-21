package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_in_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [IN, EOF] for \"in\"" in {
    val tokens = tokenize("in", collector)

    tokens shouldBe asList(IN.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"in_\"" in {
    val tokens = tokenize("in_", collector)

    tokens shouldBe asList(IDENT.T("in_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"inX\"" in {
    val tokens = tokenize("inX", collector)

    tokens shouldBe asList(IDENT.T("inX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"inin\"" in {
    val tokens = tokenize("inin", collector)

    tokens shouldBe asList(IDENT.T("inin"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"in0\"" in {
    val tokens = tokenize("in0", collector)

    tokens shouldBe asList(IDENT.T("in0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IN, PLUS, EOF] for \"in+\"" in {
    val tokens = tokenize("in+", collector)

    tokens shouldBe asList(IN.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IN, ERROR, EOF] for \"in$\"" in {
    val tokens = tokenize("in$", collector)

    tokens shouldBe asList(IN.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
