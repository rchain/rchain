package coop.rchain.rholang

import cats.effect.Bracket
import cats.effect.concurrent.Ref

package object interpreter {

  type _bracket[F[_]] = Bracket[F, Throwable]

  def _bracket[F[_]](implicit ev: _bracket[F]): _bracket[F] = ev

  type errorRef[F[_]] = Ref[F, Option[Throwable]]

}
