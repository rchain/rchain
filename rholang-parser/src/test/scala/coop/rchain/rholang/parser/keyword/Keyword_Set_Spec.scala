package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_Set_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [Set, EOF] for \"Set\"" in {
    val tokens = tokenize("Set", collector)

    tokens shouldBe asList(SET.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Set_\"" in {
    val tokens = tokenize("Set_", collector)

    tokens shouldBe asList(IDENT.T("Set_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"SetX\"" in {
    val tokens = tokenize("SetX", collector)

    tokens shouldBe asList(IDENT.T("SetX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"SetSet\"" in {
    val tokens = tokenize("SetSet", collector)

    tokens shouldBe asList(IDENT.T("SetSet"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"Set0\"" in {
    val tokens = tokenize("Set0", collector)

    tokens shouldBe asList(IDENT.T("Set0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [SET, PLUS, EOF] for \"Set+\"" in {
    val tokens = tokenize("Set+", collector)

    tokens shouldBe asList(SET.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [SET, ERROR, EOF] for \"Set$\"" in {
    val tokens = tokenize("Set$", collector)

    tokens shouldBe asList(SET.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
