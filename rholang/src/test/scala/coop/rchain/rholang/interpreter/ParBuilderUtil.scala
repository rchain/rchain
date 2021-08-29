package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}
import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.interpreter.builder.{ADTBuilder, ASTBuilder}
import monix.eval.Coeval

object ParBuilderUtil {
  def mkTerm(rho: String): Either[Throwable, Par] =
    ADTBuilder[Coeval, String, Par].buildWithEnv(rho, Map.empty[String, Par]).runAttempt

  def buildNormalizedTerm[F[_]: Sync](rho: String): F[Par] =
    ADTBuilder[F, String, Par].buildWithEnv(rho, Map.empty[String, Par])

  def buildNormalizedTerm[F[_]: Sync](reader: Reader): F[Par] =
    ADTBuilder[F, Reader, Par].buildWithEnv(reader, Map.empty[String, Par])

  def buildPar[F[_]: Sync](proc: Proc): F[Par] =
    ADTBuilder[F, Proc, Par].buildWithEnv(proc, Map.empty[String, Par])

  def buildAST[F[_]: Sync](rho: String): F[Proc] =
    ASTBuilder[F, Reader, Proc].build(new StringReader(rho))

}
