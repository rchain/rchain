package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_Int_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [INT, EOF] for \"Int\"" in {
    val tokens = tokenize("Int", collector)

    tokens shouldBe asList(INT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Int_\"" in {
    val tokens = tokenize("Int_", collector)

    tokens shouldBe asList(IDENT.T("Int_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"IntX\"" in {
    val tokens = tokenize("IntX", collector)

    tokens shouldBe asList(IDENT.T("IntX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"IntInt\"" in {
    val tokens = tokenize("IntInt", collector)

    tokens shouldBe asList(IDENT.T("IntInt"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Int0\"" in {
    val tokens = tokenize("Int0", collector)

    tokens shouldBe asList(IDENT.T("Int0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [INT, PLUS, EOF] for \"Int+\"" in {
    val tokens = tokenize("Int+", collector)

    tokens shouldBe asList(INT.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [INT, ERROR, EOF] for \"Int$\"" in {
    val tokens = tokenize("Int$", collector)

    tokens shouldBe asList(INT.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
