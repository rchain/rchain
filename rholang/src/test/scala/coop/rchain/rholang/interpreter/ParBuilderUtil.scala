package coop.rchain.rholang.interpreter

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.Compiler
import monix.eval.Coeval
import org.scalatest.EitherValues._
import org.scalatest.Assertion
import org.scalatest.Matchers.convertToAnyShouldWrapper

object ParBuilderUtil {

  def mkTerm(rho: String): Either[Throwable, Par] =
    Compiler[Coeval].sourceToADT(rho, Map.empty[String, Par]).runAttempt

  def assertCompiledEqual(s: String, t: String): Assertion =
    ParBuilderUtil.mkTerm(s).right.value shouldBe ParBuilderUtil.mkTerm(t).right.value

}
