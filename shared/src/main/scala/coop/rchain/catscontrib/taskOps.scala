package coop.rchain.catscontrib

import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.concurrent.Await
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A])(implicit scheduler: SchedulerService) {
    def unsafeRunSync: A = {
      println("running...")
      val cancellable = task.runAsync
      sys.addShutdownHook {
        println("cancelling...")
        cancellable.cancel()
        println("cancelled")
        import monix.execution.Scheduler.Implicits.global
        println("shutting down")
        scheduler.shutdown
        println("awaiting termination")
        val termination = scheduler.awaitTermination(30.seconds, global)
        Await.result(termination, Duration.Inf)
        println("done")
      }
      Await.result(cancellable, Duration.Inf)
    }
  }
}
