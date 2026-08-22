package coop.rchain.catscontrib.effect

import cats._
import cats.effect.ExitCase.{Completed, Error}
import cats.effect._

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

package object implicits {

  // this is for testing purposes, do not use in production code!
  implicit val concurrentId: Concurrent[Id] =
    new Concurrent[Id] {
      override def start[A](fa: Id[A]): Id[Fiber[Id, A]] = ???
      override def racePair[A, B](
          fa: Id[A],
          fb: Id[B]
      ): Id[Either[(A, Fiber[Id, B]), (Fiber[Id, A], B)]]                          = ???
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Id[A]      = ???
      override def asyncF[A](k: (Either[Throwable, A] => Unit) => Id[Unit]): Id[A] = ???
      override def suspend[A](thunk: => Id[A]): Id[A]                              = syncId.defer(thunk)
      override def bracketCase[A, B](acquire: Id[A])(use: A => Id[B])(
          release: (A, ExitCase[Throwable]) => Id[Unit]
      ): Id[B]                                                           = syncId.bracketCase(acquire)(use)(release)
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B]        = syncId.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = syncId.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): Id[A]                    = syncId.raiseError(e)
      override def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] =
        syncId.handleErrorWith(fa)(f)
      override def pure[A](x: A): Id[A] = syncId.pure(x)
    }

  implicit val syncId: Sync[Id] =
    new Sync[Id] {
      def pure[A](x: A): cats.Id[A] = x

      def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] =
        try {
          fa
        } catch {
          case NonFatal(e) => f(e)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def raiseError[A](e: Throwable): cats.Id[A] = throw e

      def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
        catsInstancesForId.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] =
        catsInstancesForId.tailRecM(a)(f)

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
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

}
