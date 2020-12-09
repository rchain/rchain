package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import monix.eval.Coeval

object ParBuilderUtil {
  def mkTerm(rho: String): Either[Throwable, Par] =
    Compiler[Coeval].buildNormalizedTerm(rho, Map.empty[String, Par]).runAttempt

  def buildNormalizedTerm[F[_]: Sync](rho: String): F[Par] =
    Compiler[F].buildNormalizedTerm(rho, Map.empty[String, Par])

  def buildNormalizedTerm[F[_]: Sync](reader: Reader): F[Par] =
    Compiler[F].buildNormalizedTerm(reader, Map.empty[String, Par])

  def buildPar[F[_]: Sync](proc: Proc): F[Par] =
    Compiler[F].buildPar(proc, Map.empty[String, Par])

  def buildAST[F[_]: Sync](rho: String): F[Proc] =
    Compiler[F].buildAST(new StringReader(rho))

}
