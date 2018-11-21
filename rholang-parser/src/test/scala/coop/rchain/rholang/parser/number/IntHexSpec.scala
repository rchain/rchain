package coop.rchain.rholang.parser.number

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class IntHexSpec extends FlatSpec with Matchers with PropertyChecks {

  val HEXs =
    Table(
      "Correct but illegal Int in HEX format",
      // ===========================
      "0x0",
      "0x9",
      "0xa",
      "0xb",
      "0xc",
      "0xd",
      "0xe",
      "0xf",
      "0xA",
      "0xB",
      "0xC",
      "0xD",
      "0xE",
      "0xF",
      "0x123456789abcdef",
      "0x123456789ABCDEF",
      //
      "0X0",
      "0X9",
      "0Xa",
      "0Xb",
      "0Xc",
      "0Xd",
      "0Xe",
      "0Xf",
      "0XA",
      "0XB",
      "0XC",
      "0XD",
      "0XE",
      "0XF",
      "0X123456789abcdef",
      "0X123456789ABCDEF",
    )

  forAll (HEXs) { hex =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(hex, collector)

    tokens shouldBe asList(ERROR.T(hex), EOF.T)

    verify(collector.getDiagnostics) eqTo
      error("lexer.err.literal.absent.int-hex-format")
        .row(1).col(1).len(hex).offset(0)
  }
}
