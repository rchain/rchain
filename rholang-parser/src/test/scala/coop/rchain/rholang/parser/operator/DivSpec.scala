package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType.{CONJUNCTION, DIV, EOF, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class DivSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [DIV, EOF] for \"/\"" in {
    val tokens = tokenize("/", collector)

    tokens shouldBe asList(DIV.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [CONJUNCTION, EOF] for \"/\\\"" in {
    val tokens = tokenize("/\\", collector)

    tokens shouldBe asList(CONJUNCTION.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, CONJUNCTION, IDENT, EOF] for \"a/\\b\"" in {
    val tokens = tokenize("a/\\b", collector)

    tokens shouldBe asList(IDENT.T("a"), CONJUNCTION.T, IDENT.T("b"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [DIV, IDENT, EOF] for \"/a\"" in {
    val tokens = tokenize("/a", collector)

    tokens shouldBe asList(DIV.T, IDENT.T("a"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, DIV, EOF] for \"a/\"" in {
    val tokens = tokenize("a/", collector)

    tokens shouldBe asList(IDENT.T("a"), DIV.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, DIV, IDENT, EOF] for \"a/b\"" in {
    val tokens = tokenize("a/b", collector)

    tokens shouldBe asList(IDENT.T("a"), DIV.T, IDENT.T("b"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
