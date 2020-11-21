package coop.rchain.monix

import cats.~>
import monix.eval.Task

trait MonixableSyntax {
  implicit final def sharedSyntaxdMonixableToTask[F[_], A](fa: F[A]): MonixableToTaskOps[F, A] =
    new MonixableToTaskOps[F, A](fa)

  implicit final def sharedSyntaxdMonixableFromTask[A](task: Task[A]): MonixableFromTaskOps[A] =
    new MonixableFromTaskOps(task)
}

final class MonixableToTaskOps[F[_], A](val fa: F[A]) extends AnyVal {
  def toTask(implicit m: Monixable[F]): Task[A] = Monixable[F].toTask(fa)
}

final class MonixableFromTaskOps[A](val task: Task[A]) extends AnyVal {
  def fromTask[F[_]: Monixable]: F[A] = Monixable[F].fromTask(task)
}
