package coop.rchain.rholang.interpreter

import java.io.Reader

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import monix.eval.Coeval

object ParBuilderUtil {
  def mkTerm(rho: String): Either[Throwable, Par] =
    ParBuilder[Coeval].buildNormalizedTerm(rho, None).runAttempt

  def buildNormalizedTerm[F[_]: Sync](rho: String): F[Par] =
    ParBuilder[F].buildNormalizedTerm(rho, None)

  def buildNormalizedTerm[F[_]: Sync](reader: Reader): F[Par] =
    ParBuilder[F].buildNormalizedTerm(reader, None)

  def buildPar[F[_]: Sync](proc: Proc): F[Par] = ParBuilder[F].buildPar(proc, None)
}
