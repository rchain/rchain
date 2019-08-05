package coop.rchain.casper.genesis.contracts

import cats.FlatMap
import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{NormalizerEnv, ParBuilder, Runtime}

object TestUtil {

  def eval[F[_]: Sync](
      code: String,
      runtime: Runtime[F],
      normalizerEnv: NormalizerEnv
  )(implicit rand: Blake2b512Random): F[Unit] =
    ParBuilder[F].buildNormalizedTerm(code, normalizerEnv) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: Runtime[F]
  )(implicit rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)(rand, Span.empty)
      _ <- runtime.reducer.phlo
    } yield ()
}
