package coop.rchain.rholang.parser.keyword

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.tokenize
import coop.rchain.rholang.parser.RhoTokenType._
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class Keyword_new_Spec extends FlatSpec with Matchers  with OneInstancePerTest {

  val collector = new DiagnosticCollector

  "RhoLexer" should "return [NEW, EOF] for \"new\"" in {
    val tokens = tokenize("new", collector)

    tokens shouldBe asList(NEW.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"new_\"" in {
    val tokens = tokenize("new_", collector)

    tokens shouldBe asList(IDENT.T("new_"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"newX\"" in {
    val tokens = tokenize("newX", collector)

    tokens shouldBe asList(IDENT.T("newX"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"newnew\"" in {
    val tokens = tokenize("newnew", collector)

    tokens shouldBe asList(IDENT.T("newnew"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [IDENT, EOF] for \"new0\"" in {
    val tokens = tokenize("new0", collector)

    tokens shouldBe asList(IDENT.T("new0"), EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NEW, PLUS, EOF] for \"new+\"" in {
    val tokens = tokenize("new+", collector)

    tokens shouldBe asList(NEW.T, PLUS.T, EOF.T)
    collector.getDiagnostics shouldBe empty
  }

  "RhoLexer" should "return [NEW, ERROR, EOF] for \"new$\"" in {
    val tokens = tokenize("new$", collector)

    tokens shouldBe asList(NEW.T, ERROR.T("$"), EOF.T)
    collector.getDiagnostics should have size 1
  }
}
