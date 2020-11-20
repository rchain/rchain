package coop.rchain.catscontrib

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.{Log, LogSource}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.Await
import scala.concurrent.duration._

object TaskContrib {

  implicit class AbstractTaskOps[F[_], A](val fa: F[A]) extends AnyVal {

    // TODO: Migrated from previous Task version `attemptAndLog` / do we really need it?
    def logOnError(msg: String)(implicit s: Sync[F], log: Log[F], ev: LogSource): F[A] =
      fa.onError { case ex => log.error(msg, ex) }

  }

  implicit class TaskOps[A](val task: Task[A]) extends AnyVal {

    // TODO: This is duplicated `Task#runSyncUnsafe` / it should be removed
    def unsafeRunSync(implicit scheduler: Scheduler): A =
      Await.result(
        task.runToFuture,
        Duration.Inf
      )

  }

}
