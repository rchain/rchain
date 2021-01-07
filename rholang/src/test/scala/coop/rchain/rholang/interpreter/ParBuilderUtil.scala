package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.ParBuilder
import coop.rchain.rholang.parser.rholang_mercury.Absyn.Proc
import monix.eval.Coeval

object ParBuilderUtil {
  def mkTerm(rho: String): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, Map.empty[String, Par]).runAttempt

  def buildNormalizedTerm[F[_]: Sync](rho: String): F[Par] =
    ParBuilder[F].buildNormalizedTerm(rho, Map.empty[String, Par])

  def buildNormalizedTerm[F[_]: Sync](reader: Reader): F[Par] =
    ParBuilder[F].buildNormalizedTerm(reader, Map.empty[String, Par])

  def buildPar[F[_]: Sync](proc: Proc): F[Par] =
    ParBuilder[F].buildPar(proc, Map.empty[String, Par])

  def buildAST[F[_]: Sync](rho: String): F[Proc] =
    ParBuilder[F].buildAST(new StringReader(rho))

}
