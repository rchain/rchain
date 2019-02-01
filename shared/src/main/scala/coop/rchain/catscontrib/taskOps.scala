package coop.rchain.catscontrib

import monix.eval.Task
import monix.execution.Scheduler
import cats.implicits._
import scala.concurrent.Await
import scala.concurrent.duration._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A]) {
    def unsafeRunSync(implicit scheduler: Scheduler): A =
      Await.result(task.runToFuture, Duration.Inf)

    // TODO should also push stacktrace to logs (not only console as it is doing right now)
    def attemptAndLog: Task[A] = task.attempt.flatMap {
      case Left(ex)      => Task.delay(ex.printStackTrace()) *> Task.raiseError[A](ex)
      case Right(result) => Task.pure(result)
    }

  }
}
