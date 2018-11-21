package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_true_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [TRUE, EOF] for \"true\"" in {
    val tokens = tokenize("true", collector)

    tokens shouldBe asList(TRUE.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"true_\"" in {
    val tokens = tokenize("true_", collector)

    tokens shouldBe asList(IDENT.T("true_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"trueX\"" in {
    val tokens = tokenize("trueX", collector)

    tokens shouldBe asList(IDENT.T("trueX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"truetrue\"" in {
    val tokens = tokenize("truetrue", collector)

    tokens shouldBe asList(IDENT.T("truetrue"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"true0\"" in {
    val tokens = tokenize("true0", collector)

    tokens shouldBe asList(IDENT.T("true0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [TRUE, PLUS, EOF] for \"true+\"" in {
    val tokens = tokenize("true+", collector)

    tokens shouldBe asList(TRUE.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [TRUE, ERROR, EOF] for \"true$\"" in {
    val tokens = tokenize("true$", collector)

    tokens shouldBe asList(TRUE.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
