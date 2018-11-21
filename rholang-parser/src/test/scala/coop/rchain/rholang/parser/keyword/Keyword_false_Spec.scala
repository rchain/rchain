package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_false_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [FALSE, EOF] for \"false\"" in {
    val tokens = tokenize("false", collector)

    tokens shouldBe asList(FALSE.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"false_\"" in {
    val tokens = tokenize("false_", collector)

    tokens shouldBe asList(IDENT.T("false_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"falseX\"" in {
    val tokens = tokenize("falseX", collector)

    tokens shouldBe asList(IDENT.T("falseX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"falsefalse\"" in {
    val tokens = tokenize("falsefalse", collector)

    tokens shouldBe asList(IDENT.T("falsefalse"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"false0\"" in {
    val tokens = tokenize("false0", collector)

    tokens shouldBe asList(IDENT.T("false0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [FALSE, PLUS, EOF] for \"false+\"" in {
    val tokens = tokenize("false+", collector)

    tokens shouldBe asList(FALSE.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [FALSE, ERROR, EOF] for \"false$\"" in {
    val tokens = tokenize("false$", collector)

    tokens shouldBe asList(FALSE.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
