package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_match_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [MATCH, EOF] for \"match\"" in {
    val tokens = tokenize("match", collector)

    tokens shouldBe asList(MATCH.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"match_\"" in {
    val tokens = tokenize("match_", collector)

    tokens shouldBe asList(IDENT.T("match_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"matchX\"" in {
    val tokens = tokenize("matchX", collector)

    tokens shouldBe asList(IDENT.T("matchX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"matchmatch\"" in {
    val tokens = tokenize("matchmatch", collector)

    tokens shouldBe asList(IDENT.T("matchmatch"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"match0\"" in {
    val tokens = tokenize("match0", collector)

    tokens shouldBe asList(IDENT.T("match0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [MATCH, PLUS, EOF] for \"match+\"" in {
    val tokens = tokenize("match+", collector)

    tokens shouldBe asList(MATCH.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [MATCH, ERROR, EOF] for \"match$\"" in {
    val tokens = tokenize("match$", collector)

    tokens shouldBe asList(MATCH.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
