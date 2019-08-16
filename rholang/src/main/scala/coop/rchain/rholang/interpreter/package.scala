package coop.rchain.rholang

import cats.effect.Bracket

package object interpreter {

  type _bracket[F[_]] = Bracket[F, Throwable]

  def _bracket[F[_]](implicit ev: _bracket[F]): _bracket[F] = ev

}
