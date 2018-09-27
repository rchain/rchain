package coop.rchain.catscontrib

import monix.eval.Task

final class TaskableOps[F[_]: Taskable, A](fa: F[A]) {
  def toTask: Task[A] = Taskable[F].toTask(fa)
}

trait ToTaskableOps {
  implicit def ToTaskableOps[F[_]: Taskable, A](fa: F[A]): TaskableOps[F, A] =
    new TaskableOps[F, A](fa)
}
