package coop.rchain.node

import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import cats.{Applicative, FlatMap}
import coop.rchain.comm.{CommError, UnknownCommError}
import monix.eval.Task

//TODO investigate if this can be extracted to shared, the problem is errors
//ATM the project does not have a notion of shared base error which this could require
object SyncInstances {
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  def syncEffect: Sync[Effect] =
    new Sync[Effect] {
      def suspend[A](thunk: => Effect[A]): Effect[A] =
        EitherT(Task.defer(thunk.value))

      def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] =
        implicitly[FlatMap[Effect]].flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] =
        implicitly[FlatMap[Effect]].tailRecM(a)(f)

      def raiseError[A](e: Throwable): Effect[A] =
        EitherT.left(Task.pure(UnknownCommError(e.getMessage)))

      def pure[A](x: A): Effect[A] = implicitly[Applicative[Effect]].pure(x)

      def bracketCase[A, B](acquire: Effect[A])(use: A => Effect[B])(
          release: (A, ExitCase[Throwable]) => Effect[Unit]): Effect[B] =
        acquire flatMap { state =>
          try {
            use(state)
          } catch {
            case t: Throwable => raiseError(t)
          } finally {
            release(state, ExitCase.Completed)
          }
        }

      def handleErrorWith[A](fa: Effect[A])(f: Throwable => Effect[A]): Effect[A] =
        EitherT(fa.value flatMap {
          case Left(commError)        => f(new Exception(s"CommError: $commError")).value
          case r: Right[CommError, A] => Task.pure(r)
        })
    }
}
