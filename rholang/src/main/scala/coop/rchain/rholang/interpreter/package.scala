package coop.rchain.rholang

import cats.effect.concurrent.Ref
import cats.syntax.all._

package object interpreter {

  type _error[F[_]] = Ref[F, Option[Throwable]]

  def _error[F[_]](implicit ev: _error[F]): _error[F] = ev

  def reportError[F[_]](error: Throwable)(implicit errorReporter: _error[F]): F[Unit] =
    errorReporter.update(_.fold(error.some)(_.some))

}
