package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class GtFailSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val ERR_CODE_ABSENT_OPERATOR = "lexer.err.operator.absent.arithmetic"

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \">>\"" in {
    val tokens = tokenize(">>", collector)

    tokens shouldBe asList(ERROR.T(">>"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, EOF] for \">>>\"" in {
    val tokens = tokenize(">>>", collector)

    tokens shouldBe asList(ERROR.T(">>>"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, GT, EOF] for \">>>>\"" in {
    val tokens = tokenize(">>>>", collector)

    tokens shouldBe asList(ERROR.T(">>>"), GT.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, ERROR, EOF] for \">>>>>\"" in {
    val tokens = tokenize(">>>>>", collector)

    tokens shouldBe asList(ERROR.T(">>>"), ERROR.T(">>"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(3).offset(0),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(4).len(2).offset(3)
    )
  }

  "RhoLexer" should "return [ERROR, ERROR, EOF] for \">>>>>>\"" in {
    val tokens = tokenize(">>>>>>", collector)

    tokens shouldBe asList(ERROR.T(">>>"), ERROR.T(">>>"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(1).len(3).offset(0),
      error(ERR_CODE_ABSENT_OPERATOR)
        .row(1).col(4).len(3).offset(3)
    )
  }
}
