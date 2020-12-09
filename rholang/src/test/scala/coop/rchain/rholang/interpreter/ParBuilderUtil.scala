package coop.rchain.rholang.interpreter

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import monix.eval.Coeval

object ParBuilderUtil {

  def mkTerm(rho: String): Either[Throwable, Par] =
    Compiler[Coeval].sourceToADT(rho, Map.empty[String, Par]).runAttempt

  def buildPar[F[_]: Sync](proc: Proc): F[Par] =
    Compiler[F].astToADT(proc, Map.empty[String, Par])

}
