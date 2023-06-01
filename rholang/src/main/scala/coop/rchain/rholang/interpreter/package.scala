package coop.rchain.rholang

import cats.effect.MonadCancel

package object interpreter {

  type _error[F[_]] = MonadCancel[F, Throwable]

  def _error[F[_]](implicit ev: _error[F]): _error[F] = ev

}
