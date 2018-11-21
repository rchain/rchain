package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_contract_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [CONTRACT, EOF] for \"contract\"" in {
    val tokens = tokenize("contract", collector)

    tokens shouldBe asList(CONTRACT.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"contract_\"" in {
    val tokens = tokenize("contract_", collector)

    tokens shouldBe asList(IDENT.T("contract_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"contractX\"" in {
    val tokens = tokenize("contractX", collector)

    tokens shouldBe asList(IDENT.T("contractX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"contractcontract\"" in {
    val tokens = tokenize("contractcontract", collector)

    tokens shouldBe asList(IDENT.T("contractcontract"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"contract0\"" in {
    val tokens = tokenize("contract0", collector)

    tokens shouldBe asList(IDENT.T("contract0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [CONTRACT, PLUS, EOF] for \"contract+\"" in {
    val tokens = tokenize("contract+", collector)

    tokens shouldBe asList(CONTRACT.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [CONTRACT, ERROR, EOF] for \"contract$\"" in {
    val tokens = tokenize("contract$", collector)

    tokens shouldBe asList(CONTRACT.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
