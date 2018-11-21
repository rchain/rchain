package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class BackSlashFailSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val ERR_CODE_ABSENT_OPERATOR = "lexer.err.operator.absent.back-slash"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \"\\\"" in {
    val tokens = tokenize("\\", collector)

    tokens shouldBe asList(ERROR.T("\\"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(1).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, EOF] for \"\\a\"" in {
    val tokens = tokenize("\\a", collector)

    tokens shouldBe asList(ERROR.T("\\"), IDENT.T("a"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(1).offset(0)
  }

  "RhoLexer" should "return [ERROR, ERROR, EOF] for \"\\\\\"" in {
    val tokens = tokenize("\\\\", collector)

    tokens shouldBe asList(ERROR.T("\\"), ERROR.T("\\"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(1).offset(0),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(1).offset(1)
    )
  }

  "RhoLexer" should "return [ERROR, ERROR, IDENT, EOF] for \"\\\\a\"" in {
    val tokens = tokenize("\\\\a", collector)

    tokens shouldBe asList(ERROR.T("\\"), ERROR.T("\\"), IDENT.T("a"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(1).offset(0),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(2).len(1).offset(1)
    )
  }
}
