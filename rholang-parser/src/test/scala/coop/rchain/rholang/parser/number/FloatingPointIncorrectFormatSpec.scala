package coop.rchain.rholang.parser.number

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class FloatingPointIncorrectFormatSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val ERROR_CODE_ABSENT_FP = "lexer.err.literal.absent.floating-point"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, IDENT, EOF] for \".0e\"" in {
    val tokens = tokenize(".0e", collector)

    tokens shouldBe asList(ERROR.T(".0"), IDENT.T("e"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, EOF] for \"0.e\"" in {
    val tokens = tokenize("0.e", collector)

    tokens shouldBe asList(ERROR.T("0."), IDENT.T("e"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, EOF] for \"0.eD\"" in {
    val tokens = tokenize("0.eD", collector)

    tokens shouldBe asList(ERROR.T("0."), IDENT.T("eD"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, PLUS, EOF] for \"0.e+\"" in {
    val tokens = tokenize("0.e+", collector)

    tokens shouldBe asList(ERROR.T("0."), IDENT.T("e"), PLUS.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, PLUS, EOF] for \"0.0e+\"" in {
    val tokens = tokenize("0.0e+", collector)

    tokens shouldBe asList(ERROR.T("0.0"), IDENT.T("e"), PLUS.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, MINUS, IDENT, EOF] for \"0.0-d\"" in {
    val tokens = tokenize("0.0-d", collector)

    tokens shouldBe asList(ERROR.T("0.0"), MINUS.T, IDENT.T("d"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, IDENT, MINUS, IDENT, EOF] for \"0.e-f\"" in {
    val tokens = tokenize("0.e-f", collector)

    tokens shouldBe asList(ERROR.T("0."), IDENT.T("e"), MINUS.T, IDENT.T("f"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERROR_CODE_ABSENT_FP)
        .row(1).col(1).len(2).offset(0)
  }
}
