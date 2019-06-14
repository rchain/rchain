package coop.rchain.shared

import cats.Applicative
import cats.effect.Sync
import scodec.{Attempt, Err}

object AttemptOpsF {

  val ex: Err ⇒ Exception = err => new Exception(err.toString)
  implicit class RichAttempt[F[_]: Sync, T](a: Attempt[T]) {

    def get: F[T] =
      a match {
        case Attempt.Successful(res) =>
          Applicative[F].pure(res)
        case Attempt.Failure(err) =>
          Sync[F].raiseError[T](ex(err))
      }
  }
}
