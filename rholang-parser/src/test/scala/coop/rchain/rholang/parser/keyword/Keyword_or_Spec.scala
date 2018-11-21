package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_or_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [OR, EOF] for \"or\"" in {
    val tokens = tokenize("or", collector)

    tokens shouldBe asList(OR.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"or_\"" in {
    val tokens = tokenize("or_", collector)

    tokens shouldBe asList(IDENT.T("or_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"orX\"" in {
    val tokens = tokenize("orX", collector)

    tokens shouldBe asList(IDENT.T("orX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"oror\"" in {
    val tokens = tokenize("oror", collector)

    tokens shouldBe asList(IDENT.T("oror"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"or0\"" in {
    val tokens = tokenize("or0", collector)

    tokens shouldBe asList(IDENT.T("or0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [OR, PLUS, EOF] for \"or+\"" in {
    val tokens = tokenize("or+", collector)

    tokens shouldBe asList(OR.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [OR, ERROR, EOF] for \"or$\"" in {
    val tokens = tokenize("or$", collector)

    tokens shouldBe asList(OR.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
