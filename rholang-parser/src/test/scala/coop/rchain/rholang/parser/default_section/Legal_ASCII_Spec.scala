package coop.rchain.rholang.parser.default_section

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.ERROR
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Check: all ASCII chars in range [32, 126] not fail in default section
  * so has special RhoLexer rules like
  * {{{_case("~", () => TILDE.T)}}}
  * or
  * {{{_case("?", err("operator.absent"))}}}
  **/
class Legal_ASCII_Spec extends FlatSpec with Matchers with PropertyChecks {

  val legalASCIIChars =
    Table(
      "legal ASCII char",
      ('\u0020' until '\u007F').map(_.toString): _*
    )

  forAll(legalASCIIChars) { asciiChar =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(asciiChar, collector)

    assert(!(
      tokens.size == 2
        && (tokens.get(0).`type` eq ERROR)
        && collector.getDiagnostics.get(0).code.startsWith("lexer.err.codepoint.")))
  }
}