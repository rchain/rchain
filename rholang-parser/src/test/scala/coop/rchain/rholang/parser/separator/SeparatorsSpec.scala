package coop.rchain.rholang.parser.separator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class SeparatorsSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, EOF] for \"()[]{}\"" in {
    val tokens = tokenize("()[]{}", collector)

    tokens shouldBe asList(
      LPAREN.T, RPAREN.T,
      LBRACKET.T, RBRACKET.T,
      LBRACE.T, RBRACE.T,
      EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [COMMA, SEMI, EOF] for \",;\"" in {
    val tokens = tokenize(",;", collector)

    tokens shouldBe asList(
      COMMA.T,
      SEMI.T,
      EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
