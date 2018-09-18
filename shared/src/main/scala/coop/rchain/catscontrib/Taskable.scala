package coop.rchain.catscontrib

import cats._
import cats.data._

import monix.eval.Task

trait Taskable[F[_]] {
  def toTask[A](fa: F[A]): Task[A]
}

object Taskable extends TaskableInstances {
  def apply[F[_]](implicit F: Taskable[F]): Taskable[F] = F
}

trait TaskableInstances extends TaskableInstances0 {
  implicit val taskTaskable: Taskable[Task] = new Taskable[Task] {
    def toTask[A](fa: Task[A]): Task[A] = fa
  }
}

sealed trait TaskableInstances0 {
  implicit def eitherTTaskable[F[_]: Monad: Taskable, E]: Taskable[EitherT[F, E, ?]] =
    new Taskable[EitherT[F, E, ?]] {
      case class ToTaskException(e: E) extends RuntimeException

      def toTask[A](fa: EitherT[F, E, A]): Task[A] =
        Taskable[F]
          .toTask(fa.value)
          .flatMap {
            case Right(a) => Task.now(a)
            case Left(e)  => Task.raiseError(ToTaskException(e))
          }

    }
}
