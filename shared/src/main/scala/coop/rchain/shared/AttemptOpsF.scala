package coop.rchain.shared

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import cats.syntax.either._
import scodec.{Attempt, Err}

object AttemptOpsF {

  val ex: String => Exception = errMsg => new Exception(errMsg)
  implicit class RichAttempt[F[_]: Sync, T](a: Attempt[T]) {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def get: F[T] =
      a match {
        case Attempt.Successful(res) =>
          Applicative[F].pure(res)
        case Attempt.Failure(err) =>
          Sync[F].raiseError[T](ex(err.messageWithContext))
      }

    def toEitherThrowable: Either[Throwable, T] =
      a.toEither.leftMap((err: Err) => new Exception(err.messageWithContext))
  }
}
