package coop.rchain.catscontrib.effect

import cats._
import cats.effect.ExitCase.{Completed, Error}
import cats.effect._
import scala.concurrent.ExecutionContext

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

package object implicits {

  implicit val syncId: Sync[Id] =
    new Sync[Id] {
      def pure[A](x: A): cats.Id[A] = x

      def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] =
        try {
          fa
        } catch {
          case NonFatal(e) => f(e)
        }

      def raiseError[A](e: Throwable): cats.Id[A] = throw e

      def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
        catsInstancesForId.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] =
        catsInstancesForId.tailRecM(a)(f)

      def bracketCase[A, B](acquire: A)(use: A => B)(release: (A, ExitCase[Throwable]) => Unit): B =
        Try(use(acquire)) match {
          case Success(result) =>
            release(acquire, ExitCase.Completed)
            result

          case Failure(e) =>
            release(acquire, ExitCase.error(e))
            throw e
        }

      def suspend[A](thunk: => A): A = thunk
    }

  implicit val bracketTry: Bracket[Try, Throwable] = new Bracket[Try, Throwable] {
    private val trySyntax = cats.implicits.catsStdInstancesForTry

    override def bracketCase[A, B](
        acquire: Try[A]
    )(use: A => Try[B])(release: (A, ExitCase[Throwable]) => Try[Unit]): Try[B] =
      acquire.flatMap(
        resource =>
          trySyntax
            .attempt(use(resource))
            .flatMap((result: Either[Throwable, B]) => {
              val releaseEff =
                result match {
                  case Left(err) => release(resource, Error(err))
                  case Right(_)  => release(resource, Completed)
                }

              trySyntax.productR(releaseEff)(result.toTry)
            })
      )

    override def raiseError[A](e: Throwable): Try[A] = trySyntax.raiseError(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      trySyntax.handleErrorWith(fa)(f)

    override def pure[A](x: A): Try[A] = trySyntax.pure(x)

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = trySyntax.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = trySyntax.tailRecM(a)(f)
  }
}
