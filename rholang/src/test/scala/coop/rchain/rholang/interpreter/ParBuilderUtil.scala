package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import monix.eval.Coeval

object ParBuilderUtil {
  def mkTerm(rho: String): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, NormalizerEnv.empty).runAttempt

  def buildNormalizedTerm[F[_]: Sync](rho: String): F[Par] =
    ParBuilder[F].buildNormalizedTerm(rho, NormalizerEnv.empty)

  def buildNormalizedTerm[F[_]: Sync](reader: Reader): F[Par] =
    ParBuilder[F].buildNormalizedTerm(reader, NormalizerEnv.empty)

  def buildPar[F[_]: Sync](proc: Proc): F[Par] = ParBuilder[F].buildPar(proc, NormalizerEnv.empty)

  def buildAST[F[_]: Sync](rho: String): F[Proc] =
    ParBuilder[F].buildAST(new StringReader(rho))

}
