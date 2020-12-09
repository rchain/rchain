package coop.rchain.casper.genesis.contracts

import cats.FlatMap
import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.compiler.Compiler

object TestUtil {
  def eval[F[_]: Sync, Env](
      source: CompiledRholangSource[Env],
      runtime: Runtime[F]
  )(implicit rand: Blake2b512Random): F[Unit] = eval(source.code, runtime, source.env)

  def eval[F[_]: Sync](
      code: String,
      runtime: Runtime[F],
      normalizerEnv: Map[String, Par]
  )(implicit rand: Blake2b512Random): F[Unit] =
    Compiler[F].buildNormalizedTerm(code, normalizerEnv) >>= (evalTerm(_, runtime))

  private def evalTerm[F[_]: FlatMap](
      term: Par,
      runtime: Runtime[F]
  )(implicit rand: Blake2b512Random): F[Unit] =
    for {
      _ <- runtime.cost.set(Cost.UNSAFE_MAX)
      _ <- runtime.reducer.inj(term)
    } yield ()
}
