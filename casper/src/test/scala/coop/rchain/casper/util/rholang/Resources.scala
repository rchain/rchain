package coop.rchain.casper.util.rholang

import cats.effect.Resource
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeManager(
      prefix: String,
      storageSize: Long = 1024 * 1024L,
      storeType: StoreType = StoreType.RSpace2
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] = {
    implicit val log: Log[Task]            = Log.log[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    mkRuntime[Task](prefix, storageSize.toLong, storeType)
      .flatMap { runtime =>
        Resource.liftF(RuntimeManager.fromRuntime[Task](runtime))
      }
  }
}
