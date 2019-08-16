package coop.rchain.rholang.interpreter.error_handling

import cats.effect.Sync
import cats.effect.concurrent.Ref

object ErrorHandling {

  def emptyError[F[_]: Sync]: F[_error[F]] = Ref[F].of[Option[Throwable]](None)

}
