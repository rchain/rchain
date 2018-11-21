package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class TildeFailSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [ERROR, EOF] for \"~>\"" in {
    val tokens = tokenize("~>", collector)

    tokens shouldBe asList(ERROR.T("~>"), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.operator.absent.arrow")
        .row(1).col(1).len(2).offset(0)
  }
}
