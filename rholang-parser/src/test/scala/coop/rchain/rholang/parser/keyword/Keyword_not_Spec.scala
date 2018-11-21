package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_not_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [NOT, EOF] for \"not\"" in {
    val tokens = tokenize("not", collector)

    tokens shouldBe asList(NOT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"not_\"" in {
    val tokens = tokenize("not_", collector)

    tokens shouldBe asList(IDENT.T("not_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"notX\"" in {
    val tokens = tokenize("notX", collector)

    tokens shouldBe asList(IDENT.T("notX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"notnot\"" in {
    val tokens = tokenize("notnot", collector)

    tokens shouldBe asList(IDENT.T("notnot"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"not0\"" in {
    val tokens = tokenize("not0", collector)

    tokens shouldBe asList(IDENT.T("not0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NOT, PLUS, EOF] for \"not+\"" in {
    val tokens = tokenize("not+", collector)

    tokens shouldBe asList(NOT.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NOT, ERROR, EOF] for \"not$\"" in {
    val tokens = tokenize("not$", collector)

    tokens shouldBe asList(NOT.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
