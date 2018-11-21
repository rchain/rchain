package coop.rchain.rholang.parser.comment

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{DIV, EOF, ERROR, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class CommentBlockOkSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [EOF] for \"/**/\"" in {
    val tokens = tokenize("/**/", collector)

    tokens shouldBe asList(EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [DIV, EOF] for \"/**//\"" in {
    val tokens = tokenize("/**//", collector)

    tokens shouldBe asList(DIV.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [EOF] for \"/**///\"" in {
    val tokens = tokenize("/**///", collector)

    tokens shouldBe asList(EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, IDENT, EOF] for \"a/*b*/c\"" in {
    val tokens = tokenize("a/*b*/c", collector)

    tokens shouldBe asList(IDENT.T("a"), IDENT.T("c"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
