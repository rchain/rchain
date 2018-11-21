package coop.rchain.rholang.parser.separator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class EqSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [EQ, EOF] for \"=\"" in {
    val tokens = tokenize("=", collector)

    tokens shouldBe asList(EQ.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [EQ_EQ, EOF] for \"==\"" in {
    val tokens = tokenize("==", collector)

    tokens shouldBe asList(EQ_EQ.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ERROR, EOF] for \"===\"" in {
    val tokens = tokenize("===", collector)

    tokens shouldBe asList(ERROR.T("==="), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.eq")
        .row(1).col(1).len(3).offset(0)
  }

  "RhoLexer" should "return [ERROR, EQ, EOF] for \"====\"" in {
    val tokens = tokenize("====", collector)

    tokens shouldBe asList(ERROR.T("==="), EQ.T, EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error("lexer.err.operator.absent.eq")
        .row(1).col(1).len(3).offset(0)
    )
  }
}
