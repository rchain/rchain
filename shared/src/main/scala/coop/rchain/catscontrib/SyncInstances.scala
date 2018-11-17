package coop.rchain.catscontrib

import cats.implicits._
import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import cats.{Applicative, FlatMap}
import monix.eval.Task

object SyncInstances {

  def syncEffect[E](
      toThrowable: E => Throwable,
      fromThrowable: Throwable => E
  ): Sync[EitherT[Task, E, ?]] = {
    type Effect[A] = EitherT[Task, E, A]
    new Sync[Effect] {
      def suspend[A](thunk: => Effect[A]): Effect[A] =
        EitherT(Task.defer(thunk.value))

      def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] =
        implicitly[FlatMap[Effect]].flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] =
        implicitly[FlatMap[Effect]].tailRecM(a)(f)

      def raiseError[A](e: Throwable): Effect[A] =
        EitherT.left(Task.pure(fromThrowable(e)))

      def pure[A](x: A): Effect[A] = implicitly[Applicative[Effect]].pure(x)

      def bracketCase[A, B](
          acquire: Effect[A]
      )(use: A => Effect[B])(release: (A, ExitCase[Throwable]) => Effect[Unit]): Effect[B] = {
        def useTask(eitherResource: Either[Throwable, Either[E, A]]): Task[Either[Throwable, B]] = {
          def go(eitherA: Either[E, A]): Task[Either[E, B]] =
            eitherA
              .traverse(use(_).value)
              .map(_.joinRight)

          eitherResource
            .leftMap(fromThrowable(_))
            .traverse(go)
            .map(_.joinRight.leftMap(toThrowable))
        }

        def releaseTask(
            exitValue: Either[Throwable, Either[E, A]],
            exitCase: ExitCase[Throwable]
        ): Task[Unit] = {
          def go(a: A): Task[Unit] =
            release(a, exitCase).value
              .flatMap {
                case Left(ex)  => Task.raiseError(toThrowable(ex))
                case Right(()) => Task.unit
              }

          exitValue
            .map(x => x.leftMap(toThrowable))
            .joinRight
            .traverse(go)
            .rethrow
        }

        EitherT(
          acquire.value.attempt
            .bracketCase(useTask)(releaseTask)
            .map(_.leftMap(fromThrowable))
        )

      }

      def handleErrorWith[A](fa: Effect[A])(f: Throwable => Effect[A]): Effect[A] =
        EitherT(fa.value flatMap {
          case Left(e)        => f(toThrowable(e)).value
          case r: Right[E, A] => Task.pure(r)
        })
    }
  }
}
