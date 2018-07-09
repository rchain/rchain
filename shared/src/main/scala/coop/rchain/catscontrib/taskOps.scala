package coop.rchain.catscontrib

import java.util.concurrent.TimeoutException
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Await
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A])(implicit scheduler: Scheduler) {
    def unsafeRunSync: A =
      Await.result(task.runAsync, Duration.Inf)

    def nonCancelingTimeout(after: FiniteDuration): Task[A] =
      nonCancelingTimeoutTo(
        after,
        Task.raiseError(new TimeoutException(s"Task timed-out after $after of inactivity")))

    def nonCancelingTimeoutTo[B >: A](after: FiniteDuration, backup: Task[B]): Task[B] =
      Task.racePair(task, Task.unit.delayExecution(after)).flatMap {
        case Left((a, _)) =>
          Task.now(a)
        case Right(_) =>
          backup
      }

  }
}
