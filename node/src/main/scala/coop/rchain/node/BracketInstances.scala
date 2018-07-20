package coop.rchain.node

import cats.data.EitherT
import cats.implicits._
import cats.effect.{Bracket, ExitCase}
import cats.{Applicative, FlatMap}
import coop.rchain.comm.CommError
import monix.eval.Task

//TODO extract to shared
object BracketInstances {
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  def bracketEffect: Bracket[Effect, CommError] =
    new Bracket[Effect, CommError] {
      def pure[A](x: A): Effect[A] = implicitly[Applicative[Effect]].pure(x)

      def handleErrorWith[A](fa: Effect[A])(f: CommError => Effect[A]): Effect[A] =
        EitherT(fa.value >>= {
          case Left(commError) => f(commError).value
          case r @ Right(_)    => Task.pure(r)
        })

      def raiseError[A](e: CommError): Effect[A] =
        EitherT.left(Task.pure(e))

      // Members declared in FlatMap
      def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] =
        implicitly[FlatMap[Effect]].flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] =
        implicitly[FlatMap[Effect]].tailRecM(a)(f)

      def bracketCase[A, B](acquire: Effect[A])(use: A => Effect[B])(
          release: (A, ExitCase[CommError]) => Effect[Unit]): Effect[B] =
        acquire >>= { state =>
          try {
            use(state)
          } finally {
            //FIXME add exception handling
            release(state, ExitCase.Completed)
          }
        }
    }
}
