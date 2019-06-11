package coop.rchain.rholang.interpreter.error

import cats.implicits._
import cats.effect.Concurrent
import coop.rchain.rholang.interpreter.of

object ErrorHandling {

  def emptyErrorHandler[F[_]: Concurrent]: F[_error[F]] =
    for {
      stateRef  <- of[F, Option[Throwable]](None)
      errorLock <- RWLock[F]
    } yield errorHandler[F](stateRef, errorLock)

}
