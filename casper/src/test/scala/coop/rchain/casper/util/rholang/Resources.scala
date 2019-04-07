package coop.rchain.casper.util.rholang

import cats.implicits._
import cats.effect.Resource
import cats.effect.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeAndManager(
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(implicit scheduler: Scheduler): Resource[Task, (Runtime[Task], RuntimeManager[Task])] = {
    implicit val log: Log[Task]            = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    for {
      runtime        <- mkRuntime(prefix)
      runtimeManager <- Resource.liftF(RuntimeManager.fromRuntime[Task](runtime))
    } yield ((runtime, runtimeManager))
  }

  def mkRuntimeManager(
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] = {
    implicit val log: Log[Task]            = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    mkRuntime(prefix)
      .flatMap { runtime =>
        Resource.liftF(RuntimeManager.fromRuntime[Task](runtime))
      }
  }
}
