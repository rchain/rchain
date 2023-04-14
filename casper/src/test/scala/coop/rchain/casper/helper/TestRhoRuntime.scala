package coop.rchain.casper.helper
import cats.Parallel
import cats.effect.{Async, Resource}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rholang.Resources.mkRuntimes
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.shared.Log

object TestRhoRuntime {
  def rhoRuntimeEff[F[_]: Log: Metrics: Span: Async: Parallel](
      initRegistry: Boolean = true
  ): Resource[F, (RhoRuntime[F], ReplayRhoRuntime[F], RhoHistoryRepository[F])] =
    mkRuntimes[F]("hash-set-casper-test-genesis-", initRegistry = initRegistry)
}
