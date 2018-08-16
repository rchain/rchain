package coop.rchain.shared

import cats.MonadError
import cats.implicits._
import cats.effect.Async
import cats.effect.concurrent.MVar

class AtomicMVar[F[_], A] private (private val underlying: MVar[F, A]) {
  def modify[B](f: A => F[(A, B)])(implicit monadError: MonadError[F, Throwable]): F[B] =
    for {
      value  <- underlying.take
      fValue <- f(value).attempt
      result <- fValue match {
                 case Right((newValue, b)) =>
                   underlying.put(newValue).map(_ => b)

                 case Left(err) =>
                   underlying.put(value) *> MonadError[F, Throwable].raiseError(err)
               }
    } yield result

  def update(f: A => F[A])(implicit monadError: MonadError[F, Throwable]): F[Unit] =
    modify(a => f(a).map((_, ())))
}

object AtomicMVar {
  def of[F[_]: Async, A](initial: A): F[AtomicMVar[F, A]] =
    MVar.uncancelableOf[F, A](initial).map(new AtomicMVar(_))
}
