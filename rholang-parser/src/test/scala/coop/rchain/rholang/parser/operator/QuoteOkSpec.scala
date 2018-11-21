package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class QuoteOkSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [QUOTE, EOF] for \"@\"" in {
    val tokens = tokenize("@", collector)

    tokens shouldBe asList(QUOTE.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [QUOTE, QUOTE, EOF] for \"@@\"" in {
    val tokens = tokenize("@@", collector)

    tokens shouldBe asList(QUOTE.T, QUOTE.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
