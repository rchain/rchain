package coop.rchain.rholang.parser.number

import java.math.BigInteger.{ONE, valueOf}
import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class IntTooBigSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [LITERAL_INT, EOF] for Long.MaxValue" in {
    val content = "" + Long.MaxValue
    val tokens = tokenize(content, collector)

    tokens shouldBe asList(LITERAL_INT.T(content), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [LITERAL_INT, EOF] for Long.MinValue" in {
    val content = "" + Long.MinValue
    val tokens = tokenize(content, collector)

    tokens shouldBe asList(MINUS.T, LITERAL_INT.T(content.substring(1)), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [ERROR, EOF] for (Long.MaxValue + 1)" in {
    val content = (BigInt(Long.MaxValue) + 1).toString
    val tokens = tokenize(content, collector)

    tokens shouldBe asList(ERROR.T(content), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.literal.int-too-big")
        .row(1).col(1).len(content).offset(0)
  }

  "RhoLexer" should "return [ERROR, EOF] for (Long.MinValue - 1)" in {
    val content = (BigInt(Long.MinValue) - 1).toString
    val tokens = tokenize(content, collector)

    tokens shouldBe asList(MINUS.T, ERROR.T(content.substring(1)), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.literal.int-too-big")
        .row(1).col(2).len(content.length - 1).offset(1)
  }
}
