package coop.rchain.rholang.parser.separator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class DotSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [DOT, EOF] for \".\"" in {
    val tokens = tokenize(".", collector)

    tokens shouldBe asList(DOT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ERROR, EOF] for \"..\"" in {
    val tokens = tokenize("..", collector)

    tokens shouldBe asList(ERROR.T(".."), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.dot-dot")
        .row(1).col(1).len(2).offset(0)
  }

  "RhoLexer" should "return [ELLIPSIS, EOF] for \"...\"" in {
    val tokens = tokenize("...", collector)

    tokens shouldBe asList(ELLIPSIS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ELLIPSIS, DOT, EOF] for \"....\"" in {
    val tokens = tokenize("....", collector)

    tokens shouldBe asList(ELLIPSIS.T, DOT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ELLIPSIS, ERROR, EOF] for \".....\"" in {
    val tokens = tokenize(".....", collector)

    tokens shouldBe asList(ELLIPSIS.T, ERROR.T(".."), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.dot-dot")
        .row(1).col(4).len(2).offset(3)
  }
}
