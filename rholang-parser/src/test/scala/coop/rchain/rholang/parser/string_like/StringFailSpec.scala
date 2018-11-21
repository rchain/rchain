package coop.rchain.rholang.parser.string_like

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class StringFailSpec extends FlatSpec with Matchers with OneInstancePerTest {

  val ERR_CODE_STRING_UNCLOSED = "lexer.err.literal.string.unclosed"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \"\"\"" in {
    val tokens = tokenize("\"", collector)

    tokens shouldBe asList(ERROR.T("\""), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(1).len(1).offset(0)
  }

  "RhoLexer" should "return [IDENT, ERROR, EOF] for \"a\"\"" in {
    val tokens = tokenize("a\"", collector)

    tokens shouldBe asList(IDENT.T("a"), ERROR.T("\""), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(2).len(1).offset(1)
  }

  "RhoLexer" should "return [ERROR, EOF] for \"\"a\"" in {
    val tokens = tokenize("\"a", collector)

    tokens shouldBe asList(ERROR.T("\"a"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [IDENT, ERROR, EOF] for \"a\"b\"" in {
    val tokens = tokenize("a\"b", collector)

    tokens shouldBe asList(IDENT.T("a"), ERROR.T("\"b"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(2).len(2).offset(1)
  }

  "RhoLexer" should "return [IDENT, ERROR, IDENT, ERROR, EOF] for \"a\"b\\nc\"d\\r\\n\"" in {
    val tokens = tokenize("a\"b\nc\"d\r\n", collector)

    tokens shouldBe asList(
      IDENT.T("a"), ERROR.T("\"b"), IDENT.T("c"), ERROR.T("\"d"), EOF.T)

    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(2).len(2).offset(1),
      error(ERR_CODE_STRING_UNCLOSED)
        .row(2).col(2).len(2).offset(5)
    )
  }

  "RhoLexer" should "return [IDENT, ERROR, IDENT, ERROR, EOF] for \"a\"b\\r\\nc\"d\\n\"" in {
    val tokens = tokenize("a\"b\r\nc\"d\n", collector)

    tokens shouldBe asList(
      IDENT.T("a"), ERROR.T("\"b"), IDENT.T("c"), ERROR.T("\"d"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_STRING_UNCLOSED)
        .row(1).col(2).len(2).offset(1),
      error(ERR_CODE_STRING_UNCLOSED)
        .row(2).col(2).len(2).offset(6)
    )
  }
}
