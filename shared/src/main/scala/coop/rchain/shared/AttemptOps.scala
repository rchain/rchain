package coop.rchain.shared

import cats.syntax.either._
import scodec.{Attempt, Err}

object AttemptOps {

  implicit class RichAttempt[T](a: Attempt[T]) {

    def get: T =
      a match {
        case Attempt.Successful(res) => res
        case Attempt.Failure(err)    => throw new Exception(err.messageWithContext)
      }

    def toEitherThrowable: Either[Throwable, T] =
      a.toEither.leftMap((err: Err) => new Exception(err.messageWithContext))
  }
}
