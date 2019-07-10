package coop.rchain.casper.util.rholang

import java.nio.file.Path

import cats.effect.{Concurrent, ContextShift, Resource}
import cats.implicits._
import cats.temp.par
import coop.rchain.metrics
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.rholang.Resources.{mkRuntimeAt, mkTempDir}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeManager(
      prefix: String,
      storageSize: Long = 1024 * 1024L
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] =
    mkTempDir[Task](prefix) >>= (mkRuntimeManagerAt(_)(storageSize))

  def mkRuntimeManagerAt[F[_]: Concurrent: par.Par: ContextShift](storageDirectory: Path)(
      storageSize: Long = 10 * 1024 * 1024L
  )(
      implicit scheduler: Scheduler
  ): Resource[F, RuntimeManager[F]] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtime        <- mkRuntimeAt[F](storageDirectory)(storageSize)
      runtimeManager <- Resource.liftF(RuntimeManager.fromRuntime(runtime))
    } yield runtimeManager
  }

}
