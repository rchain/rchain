package coop.rchain.rholang.parser.separator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class ColonSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [COLON, EOF] for \":\"" in {
    val tokens = tokenize(":", collector)

    tokens shouldBe asList(COLON.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ERROR, EOF] for \"::\"" in {
    val tokens = tokenize("::", collector)

    tokens shouldBe asList(ERROR.T("::"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.colon-colon")
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, COLON, EOF] for \":::\"" in {
    val tokens = tokenize(":::", collector)

    tokens shouldBe asList(ERROR.T("::"), COLON.T, EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.colon-colon")
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ERROR, ERROR, EOF] for \"::::\"" in {
    val tokens = tokenize("::::", collector)

    tokens shouldBe asList(ERROR.T("::"), ERROR.T("::"), EOF.T)
    verify(collector.getDiagnostics).eqTo(
      error("lexer.err.operator.absent.colon-colon")
        .row(1).col(1).len(2).offset(0),
      error("lexer.err.operator.absent.colon-colon")
        .row(1).col(3).len(2).offset(2)
    )
  }
}
