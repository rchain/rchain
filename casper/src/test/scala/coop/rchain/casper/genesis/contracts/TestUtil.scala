package coop.rchain.casper.genesis.contracts

import cats.FlatMap
import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{ParBuilder, RhoRuntime}

object TestUtil {
  def eval[F[_]: Sync, Env](
      source: CompiledRholangSource[Env],
      runtime: RhoRuntime[F]
  )(implicit rand: Blake2b512Random): F[Unit] = eval(source.code, runtime, source.env)

  def eval[F[_]: Sync](
      code: String,
      runtime: RhoRuntime[F],
      normalizerEnv: Map[String, Par]
  )(implicit rand: Blake2b512Random): F[Unit] =
    ParBuilder[F].buildNormalizedTerm(code, normalizerEnv) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: RhoRuntime[F]
  )(implicit rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.cost.set(Cost.UNSAFE_MAX)
      _ <- runtime.inj(term)
    } yield ()
}
