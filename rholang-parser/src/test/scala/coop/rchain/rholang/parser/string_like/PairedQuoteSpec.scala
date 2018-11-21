package coop.rchain.rholang.parser.string_like

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class PairedQuoteSpec extends FlatSpec with Matchers with OneInstancePerTest {

  val ERR_CODE_ABSENT_LITERAL = "lexer.err.literal.absent.single-quote"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \"''\"" in {
    val tokens = tokenize("''", collector)

    tokens shouldBe asList(
      ERROR.T("''"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_LITERAL)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, EOF] for \"'a'\"" in {
    val tokens = tokenize("'a'", collector)

    tokens shouldBe asList(
      ERROR.T("'a'"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_LITERAL)
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, EOF] for \"'ab'\"" in {
    val tokens = tokenize("'ab'", collector)

    tokens shouldBe asList(
      ERROR.T("'ab'"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_LITERAL)
        .row(1).col(1).len(4).offset(0)
  }

  "RhoLexer" should "return [IDENT, ERROR, IDENT, EOF] for \"a'b'c\"" in {
    val tokens = tokenize("a'b'c", collector)

    tokens shouldBe asList(
      IDENT.T("a"), ERROR.T("'b'"), IDENT.T("c"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_LITERAL)
        .row(1).col(2).len(3).offset(1)
  }

  "RhoLexer" should "return [IDENT, ERROR, IDENT, EOF] for \"ab'cd'ef\"" in {
    val tokens = tokenize("ab'cd'ef", collector)

    tokens shouldBe asList(
      IDENT.T("ab"), ERROR.T("'cd'"), IDENT.T("ef"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_LITERAL)
        .row(1).col(3).len(4).offset(2)
  }
}






















