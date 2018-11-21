package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_Uri_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [URI, EOF] for \"Uri\"" in {
    val tokens = tokenize("Uri", collector)

    tokens shouldBe asList(URI.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Uri_\"" in {
    val tokens = tokenize("Uri_", collector)

    tokens shouldBe asList(IDENT.T("Uri_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"UriX\"" in {
    val tokens = tokenize("UriX", collector)

    tokens shouldBe asList(IDENT.T("UriX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"UriUri\"" in {
    val tokens = tokenize("UriUri", collector)

    tokens shouldBe asList(IDENT.T("UriUri"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Uri0\"" in {
    val tokens = tokenize("Uri0", collector)

    tokens shouldBe asList(IDENT.T("Uri0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [URI, PLUS, EOF] for \"Uri+\"" in {
    val tokens = tokenize("Uri+", collector)

    tokens shouldBe asList(URI.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [URI, ERROR, EOF] for \"Uri$\"" in {
    val tokens = tokenize("Uri$", collector)

    tokens shouldBe asList(URI.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
