package coop.rchain.casper.util.rholang
import cats.effect.Resource
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler

object Resources {
  def mkRuntimeManager(
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] = {
    implicit val log: Log[Task] = new Log.NOPLog[Task]

    mkRuntime(prefix)
      .flatMap { runtime =>
        Resource.make(RuntimeManager.fromRuntime[Task](runtime))(
          _ => Task.unit /* FIXME close the manager */
        )
      }
  }
}
