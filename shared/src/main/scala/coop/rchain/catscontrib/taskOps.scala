package coop.rchain.catscontrib

import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Await
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A])(implicit scheduler: Scheduler) {
    def unsafeRunSync: A =
      Await.result(task.runAsync, Duration.Inf)
  }
}
