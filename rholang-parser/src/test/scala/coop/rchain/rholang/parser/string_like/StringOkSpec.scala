package coop.rchain.rholang.parser.string_like

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class StringOkSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [LITERAL_STRING, EOF] for \"\"\"\"" in {
    val tokens = tokenize("\"\"", collector)

    tokens shouldBe asList(LITERAL_STRING.T(""), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [LITERAL_STRING, EOF] for \"\"a\"\"" in {
    val tokens = tokenize("\"a\"", collector)

    tokens shouldBe asList(LITERAL_STRING.T("a"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, LITERAL_STRING, IDENT, EOF] for \"a\"b\"c\"" in {
    val tokens = tokenize("a\"b\"c", collector)

    tokens shouldBe asList(
      IDENT.T("a"), LITERAL_STRING.T("b"), IDENT.T("c"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
