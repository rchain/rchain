package coop.rchain.rholang.parser.number

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, ERROR}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class FloatingPointCorrectFormatSpec extends FlatSpec with Matchers with PropertyChecks {

  val fps =
    Table(
      "Correct but illegal FP format",
      // ===========================
      "0f",
      "0F",
      "0d",
      "0D",
      //
      "11f",
      "11F",
      "11d",
      "11D",
      //
      "0.0",
      "0.0f",
      "0.0F",
      "0.0d",
      "0.0D",
      //
      "11.11",
      "11.11f",
      "11.11F",
      "11.11d",
      "11.11D",
      //
      "1.",
      "1.f",
      "1.F",
      "1.d",
      "1.D",
      //
      ".1",
      ".1f",
      ".1F",
      ".1d",
      ".1D",
      //
      ".11",
      ".11f",
      ".11F",
      ".11d",
      ".11D",
      //
      "11.",
      "11.f",
      "11.F",
      "11.d",
      "11.D",
      //
      "1e1",
      "1e1f",
      "1e1F",
      "1e1d",
      "1e1D",
      //
      "1e+1",
      "1e+1f",
      "1e+1F",
      "1e+1d",
      "1e+1D",
      //
      "1e-1",
      "1e-1f",
      "1e-1F",
      "1e-1d",
      "1e-1D",
      //
      "1.1e1",
      "1.1e1f",
      "1.1e1F",
      "1.1e1d",
      "1.1e1D",
      //
      "1.1e+1",
      "1.1e+1f",
      "1.1e+1F",
      "1.1e+1d",
      "1.1e+1D",
      //
      "1.1e-1",
      "1.1e-1f",
      "1.1e-1F",
      "1.1e-1d",
      "1.1e-1D",
      //
      ".1e1",
      ".1e1f",
      ".1e1F",
      ".1e1d",
      ".1e1D",
      //
      ".1e+1",
      ".1e+1f",
      ".1e+1F",
      ".1e+1d",
      ".1e+1D",
      //
      ".1e-1",
      ".1e-1f",
      ".1e-1F",
      ".1e-1d",
      ".1e-1D",
      //
      "1.e1",
      "1.e1f",
      "1.e1F",
      "1.e1d",
      "1.e1D",
      //
      "1.e+1",
      "1.e+1f",
      "1.e+1F",
      "1.e+1d",
      "1.e+1D",
      //
      "1.e-1",
      "1.e-1f",
      "1.e-1F",
      "1.e-1d",
      "1.e-1D",
    )

  forAll (fps) { fp =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(fp, collector)

    tokens shouldBe asList(ERROR.T(fp), EOF.T)

    verify(collector.getDiagnostics) eqTo
      error("lexer.err.literal.absent.floating-point")
        .row(1).col(1).len(fp).offset(0)
  }
}
