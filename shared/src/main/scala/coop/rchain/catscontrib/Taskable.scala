package coop.rchain.catscontrib

import monix.eval.Task

trait Taskable[F[_]] {
  def toTask[A](fa: F[A]): Task[A]
}

object Taskable extends TaskableInstances {
  def apply[F[_]](implicit F: Taskable[F]): Taskable[F] = F
}

trait TaskableInstances {
  implicit val taskTaskable: Taskable[Task] = new Taskable[Task] {
    def toTask[A](fa: Task[A]): Task[A] = fa
  }
}
