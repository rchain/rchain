package coop.rchain.catscontrib

import java.util.concurrent.TimeoutException

import monix.eval.Task
import monix.execution.Scheduler
import cats.implicits._
import scala.concurrent.Await
import scala.concurrent.duration._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A]) {
    def unsafeRunSync(implicit scheduler: Scheduler): A =
      Await.result(task.runToFuture, Duration.Inf)

    def nonCancelingTimeout(after: FiniteDuration): Task[A] =
      nonCancelingTimeoutTo(
        after,
        Task.raiseError(new TimeoutException(s"Task timed-out after $after of inactivity"))
      )

    def nonCancelingTimeoutTo[B >: A](after: FiniteDuration, backup: Task[B]): Task[B] =
      Task.racePair(task, Task.unit.delayExecution(after)).flatMap {
        case Left((a, _)) =>
          Task.now(a)
        case Right(_) =>
          backup
      }

    // TODO should also push stacktrace to logs (not only console as it is doing right now)
    def attemptAndLog: Task[A] = task.attempt.flatMap {
      case Left(ex)      => Task.delay(ex.printStackTrace()) *> Task.raiseError[A](ex)
      case Right(result) => Task.pure(result)
    }

  }
}
