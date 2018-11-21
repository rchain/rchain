package coop.rchain.rholang.parser.number

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class IntWithLSuffixSpec extends FlatSpec with Matchers with PropertyChecks {

  val ERROR_CODE_ABSENT_LITERAL = "lexer.err.literal.absent.int-L-suffix"

  val intsWithL =
    Table(
      ("Correct but illegal Int with L/l suffix format", "error"),
      // ===========================
      ("0l", "0l"),
      ("0L", "0L"),
      ("+0l", "0l"),
      ("+0L", "0L"),
      ("-0l", "0l"),
      ("-0L", "0L"),
      //
      ("42l", "42l"),
      ("42L", "42L"),
      ("+42l", "42l"),
      ("+42L", "42L"),
      ("-42l", "42l"),
      ("-42L", "42L")
    )

  forAll(intsWithL) { (input, literal) =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(input, collector)

    if (input.startsWith("-")) {
      tokens shouldBe asList(MINUS.T, ERROR.T(literal), EOF.T)
      verify(collector.getDiagnostics) eqTo
        error(ERROR_CODE_ABSENT_LITERAL)
          .row(1).col(2).len(literal).offset(1)
    } else if (input.startsWith("+")) {
      tokens shouldBe asList(PLUS.T, ERROR.T(literal), EOF.T)
      verify(collector.getDiagnostics) eqTo
        error(ERROR_CODE_ABSENT_LITERAL)
          .row(1).col(2).len(literal).offset(1)
    } else {
      tokens shouldBe asList(ERROR.T(literal), EOF.T)
      verify(collector.getDiagnostics) eqTo
        error(ERROR_CODE_ABSENT_LITERAL)
          .row(1).col(1).len(literal).offset(0)
    }
  }
}
