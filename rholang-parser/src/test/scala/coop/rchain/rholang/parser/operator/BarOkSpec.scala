package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class BarOkSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [WILDCARD, EOF] for \"_\"" in {
    val tokens = tokenize("_", collector)

    tokens shouldBe asList(WILDCARD.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"__\"" in {
    val tokens = tokenize("__", collector)

    tokens shouldBe asList(IDENT.T("__"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"_a\"" in {
    val tokens = tokenize("_a", collector)

    tokens shouldBe asList(IDENT.T("_a"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"a_\"" in {
    val tokens = tokenize("a_", collector)

    tokens shouldBe asList(IDENT.T("a_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"a_b\"" in {
    val tokens = tokenize("a_b", collector)

    tokens shouldBe asList(IDENT.T("a_b"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
