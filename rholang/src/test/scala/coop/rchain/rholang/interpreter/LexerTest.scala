package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.errors.LexerError
import monix.eval.Coeval
import org.scalatest.EitherValues._
import org.scalatest.{FlatSpec, Matchers}

class LexerTest extends FlatSpec with Matchers {

  def attemptMkTerm(input: String): Either[Throwable, Par] =
    Compiler[Coeval].sourceToADT(input).runAttempt()

  "Lexer" should "return LexerError for unterminated string at EOF" in {
    val attempt = attemptMkTerm("""{{ @"ack!(0) }}""")
    attempt.left.value shouldBe a[LexerError]
  }

  it should "return LexerError for unterminated string on particular line" in {
    val attempt = attemptMkTerm("{{ @\"ack\n!(0) }}")
    attempt.left.value shouldBe a[LexerError]
  }

  it should "return LexerError for illegal character" in {
    val attempt =
      attemptMkTerm("""("x is ${value}" ^ {"value" : x})""")
    attempt.left.value shouldBe a[LexerError]
  }
}
