package coop.rchain.rholang.parser.default_section

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Check: all Illegal ASCII chars ([0..31]\[9,10,12,13] + [127]) fail in RhoLexer default section.
  */
class Illegal_ASCII_Spec extends FlatSpec with Matchers with PropertyChecks {

  val x: Tuple1[String] = Tuple1("a")

  val illegalASCIIChars =
    Table(
      "illegal ASCII char",
      "\u0000",
      "\u0001",
      "\u0002",
      "\u0003",
      "\u0004",
      "\u0005",
      "\u0006",
      "\u0007",
      "\u0008",
      //                ("\t"),
      //                ("\n"),
      "\u000B",
      //                ("\f"),
      //                ("\r"),
      "\u000E",
      "\u000F",
      //
      "\u0010",
      "\u0011",
      "\u0012",
      "\u0013",
      "\u0014",
      "\u0015",
      "\u0016",
      "\u0017",
      "\u0018",
      "\u0019",
      "\u001A",
      "\u001B",
      "\u001C",
      "\u001D",
      "\u001E",
      "\u001F",
      //
      "\u007F",
    )

  forAll (illegalASCIIChars) { illegalASCIIChar =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(illegalASCIIChar, collector)

    tokens shouldBe asList(ERROR.T(illegalASCIIChar), EOF.T)
    verify(collector.getDiagnostics) eqTo
      error("lexer.err.codepoint.illegal.ascii")
        .row(1).col(1).len(1).offset(0)
  }
}
