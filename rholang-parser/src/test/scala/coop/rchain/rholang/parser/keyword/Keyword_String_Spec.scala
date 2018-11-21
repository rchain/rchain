package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_String_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [STRING, EOF] for \"String\"" in {
    val tokens = tokenize("String", collector)

    tokens shouldBe asList(STRING.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"String_\"" in {
    val tokens = tokenize("String_", collector)

    tokens shouldBe asList(IDENT.T("String_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"StringX\"" in {
    val tokens = tokenize("StringX", collector)

    tokens shouldBe asList(IDENT.T("StringX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"StringString\"" in {
    val tokens = tokenize("StringString", collector)

    tokens shouldBe asList(IDENT.T("StringString"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"String0\"" in {
    val tokens = tokenize("String0", collector)

    tokens shouldBe asList(IDENT.T("String0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [STRING, PLUS, EOF] for \"String+\"" in {
    val tokens = tokenize("String+", collector)

    tokens shouldBe asList(STRING.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [STRING, ERROR, EOF] for \"String$\"" in {
    val tokens = tokenize("String$", collector)

    tokens shouldBe asList(STRING.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
