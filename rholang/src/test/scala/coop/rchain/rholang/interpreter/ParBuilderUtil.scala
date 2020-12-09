package coop.rchain.rholang.interpreter

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.Compiler
import monix.eval.Coeval

object ParBuilderUtil {

  def mkTerm(rho: String): Either[Throwable, Par] =
    Compiler[Coeval].sourceToADT(rho, Map.empty[String, Par]).runAttempt

}
