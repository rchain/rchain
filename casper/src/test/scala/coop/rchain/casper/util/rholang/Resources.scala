package coop.rchain.casper.util.rholang

import cats.effect.Resource
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeManager(
      prefix: String,
      storageSize: Long = 1024 * 1024L
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] = {
    implicit val log: Log[Task]            = Log.log[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

    mkRuntime[Task](prefix, storageSize.toLong)
      .flatMap { runtime =>
        Resource.liftF(RuntimeManager.fromRuntime[Task](runtime))
      }
  }
}
