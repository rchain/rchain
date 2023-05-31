package coop.rchain.rholang.interpreter

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.Compiler
import cats.Eval
import org.scalatest.EitherValues._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import coop.rchain.catscontrib.effect.implicits.sEval
import cats.syntax.all._

object ParBuilderUtil {

  def mkTerm(rho: String): Either[Throwable, Par] =
    Compiler[Eval].sourceToADT(rho, Map.empty[String, Par]).attempt.value

  def assertCompiledEqual(s: String, t: String): Assertion =
    ParBuilderUtil.mkTerm(s).value shouldBe ParBuilderUtil.mkTerm(t).value

}
