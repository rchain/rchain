package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_and_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [AND, EOF] for \"and\"" in {
    val tokens = tokenize("and", collector)

    tokens shouldBe asList(AND.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"and_\"" in {
    val tokens = tokenize("and_", collector)

    tokens shouldBe asList(IDENT.T("and_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"andX\"" in {
    val tokens = tokenize("andX", collector)

    tokens shouldBe asList(IDENT.T("andX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"andand\"" in {
    val tokens = tokenize("andand", collector)

    tokens shouldBe asList(IDENT.T("andand"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"and0\"" in {
    val tokens = tokenize("and0", collector)

    tokens shouldBe asList(IDENT.T("and0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [AND, PLUS, EOF] for \"and+\"" in {
    val tokens = tokenize("and+", collector)

    tokens shouldBe asList(AND.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [AND, ERROR, EOF] for \"and$\"" in {
    val tokens = tokenize("and$", collector)

    tokens shouldBe asList(AND.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
