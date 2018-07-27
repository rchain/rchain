package coop.rchain.catscontrib.effect

import cats._
import cats.implicits._
import cats.effect.{ExitCase, Sync}

import scala.util.control.NonFatal

package object implicits {

  implicit val syncId: Sync[Id] =
    new Sync[Id] {
      def pure[A](x: A): cats.Id[A] = x

      def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] =
        try { fa } catch {
          case NonFatal(e) => f(e)
        }

      def raiseError[A](e: Throwable): cats.Id[A] = throw e

      def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
        catsInstancesForId.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] =
        catsInstancesForId.tailRecM(a)(f)

      def bracketCase[A, B](acquire: A)(use: A => B)(
          release: (A, ExitCase[Throwable]) => Unit): B = {
        var maybeErrorCase: Option[ExitCase[Throwable]] = None
        try {
          use(acquire)
        } catch {
          case NonFatal(e) => maybeErrorCase = Some(ExitCase.error(e)); throw e;
        } finally {
          release(acquire, maybeErrorCase.getOrElse(ExitCase.Completed))
        }
      }

      def suspend[A](thunk: => A): A = thunk
    }

}
