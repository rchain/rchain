package coop.rchain.shared

import cats.Applicative
import cats.effect.Sync
import scodec.{Attempt, Err}

object AttemptOpsF {

  // Exception in case of failure
  def ex(err: Err) = new Exception(err.toString)

  implicit class RichAttempt[T](a: Attempt[T]) {
    // Get result or applicative error if failure
    def get[F[_]](implicit f: Sync[F]): F[T] =
      a match {
        case Attempt.Successful(res) =>
          Applicative[F].pure(res)
        case Attempt.Failure(err) =>
          Sync[F].raiseError[T](ex(err))
      }
  }
}
