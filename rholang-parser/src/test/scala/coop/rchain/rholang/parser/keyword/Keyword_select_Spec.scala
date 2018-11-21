package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_select_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [SELECT, EOF] for \"select\"" in {
    val tokens = tokenize("select", collector)

    tokens shouldBe asList(SELECT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"select_\"" in {
    val tokens = tokenize("select_", collector)

    tokens shouldBe asList(IDENT.T("select_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"selectX\"" in {
    val tokens = tokenize("selectX", collector)

    tokens shouldBe asList(IDENT.T("selectX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"selectselect\"" in {
    val tokens = tokenize("selectselect", collector)

    tokens shouldBe asList(IDENT.T("selectselect"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"select0\"" in {
    val tokens = tokenize("select0", collector)

    tokens shouldBe asList(IDENT.T("select0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [SELECT, PLUS, EOF] for \"select+\"" in {
    val tokens = tokenize("select+", collector)

    tokens shouldBe asList(SELECT.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [SELECT, ERROR, EOF] for \"select$\"" in {
    val tokens = tokenize("select$", collector)

    tokens shouldBe asList(SELECT.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
