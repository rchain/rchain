package coop.rchain.shared

import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService

object RChainScheduler {
  val availableProcessors: Int = java.lang.Runtime.getRuntime.availableProcessors()
  // TODO: make it configurable
  // TODO: fine tune this
  val interpreterScheduler: SchedulerService = Scheduler.forkJoin(
    name = "interpreter-rspace",
    parallelism = availableProcessors * 2,
    maxThreads = availableProcessors * 2,
    reporter = UncaughtExceptionLogger
  )
}
