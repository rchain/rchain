package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR, IDENT, PLUS}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class AmpSpec extends FlatSpec with Matchers with OneInstancePerTest {

  val ERR_CODE_ABSENT_OPERATOR = "lexer.err.operator.absent.logic"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \"&\"" in {
    val tokens = tokenize("&", collector)

    tokens shouldBe asList(ERROR.T("&"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(1).offset(0)
  }

  "RhoLexer" should "return [IDENT, ERROR, PLUS, EOF] for \"a&+\"" in {
    val tokens = tokenize("a&+", collector)

    tokens shouldBe asList(IDENT.T("a"), ERROR.T("&"), PLUS.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(1).offset(1)
  }

  "RhoLexer" should "return [PLUS, ERROR, IDENT, EOF] for \"+&a\"" in {
    val tokens = tokenize("+&a", collector)

    tokens shouldBe asList(PLUS.T, ERROR.T("&"), IDENT.T("a"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(1).offset(1)
  }

  "RhoLexer" should "return [ERROR, EOF] for \"&&\"" in {
    val tokens = tokenize("&&", collector)

    tokens shouldBe asList(ERROR.T("&&"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [IDENT, ERROR, PLUS, EOF] for \"a&&+\"" in {
    val tokens = tokenize("a&&+", collector)

    tokens shouldBe asList(IDENT.T("a"), ERROR.T("&&"), PLUS.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(2).offset(1)
  }

  "RhoLexer" should "return [PLUS, ERROR, IDENT, EOF] for \"+&&a\"" in {
    val tokens = tokenize("+&&a", collector)

    tokens shouldBe asList(PLUS.T, ERROR.T("&&"), IDENT.T("a"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(2).offset(1)
  }

  "RhoLexer" should "return [ERROR, ERROR, EOF] for \"&&&\"" in {
    val tokens = tokenize("&&&", collector)

    tokens shouldBe asList(ERROR.T("&&"), ERROR.T("&"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(2).offset(0),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(3).len(1).offset(2)
    )
  }

  "RhoLexer" should "return [IDENT, ERROR, ERROR, PLUS, EOF] for \"a&&&+\"" in {
    val tokens = tokenize("a&&&+", collector)

    tokens shouldBe asList(IDENT.T("a"), ERROR.T("&&"), ERROR.T("&"), PLUS.T, EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(2).offset(1),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(4).len(1).offset(3)
    )
  }

  "RhoLexer" should "return [PLUS, ERROR, ERROR, IDENT, EOF] for \"+&&&a\"" in {
    val tokens = tokenize("+&&&a", collector)

    tokens shouldBe asList(PLUS.T, ERROR.T("&&"), ERROR.T("&"), IDENT.T("a"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(2).offset(1),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(4).len(1).offset(3)
    )
  }
}
