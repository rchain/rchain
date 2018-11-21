package coop.rchain.rholang.parser.operator

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class BackSlashOkSpec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [DISJUNCTION, EOF] for \"\\/\"" in {
    val tokens = tokenize("\\/", collector)

    tokens shouldBe asList(DISJUNCTION.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, DISJUNCTION, PLUS, EOF] for \"a\\/+\"" in {
    val tokens = tokenize("a\\/+", collector)

    tokens shouldBe asList(IDENT.T("a"), DISJUNCTION.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [PLUS, DISJUNCTION, IDENT, EOF] for \"+\\/a\"" in {
    val tokens = tokenize("+\\/a", collector)

    tokens shouldBe asList(PLUS.T, DISJUNCTION.T, IDENT.T("a"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }
}
