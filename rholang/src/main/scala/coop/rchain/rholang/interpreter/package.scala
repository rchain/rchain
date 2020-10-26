package coop.rchain.rholang

import cats.effect.Bracket
import coop.rchain.rholang.interpreter.RhoRuntimeSyntax

package object interpreter {

  type _error[F[_]] = Bracket[F, Throwable]

  def _error[F[_]](implicit ev: _error[F]): _error[F] = ev
  object syntax extends AllSyntaxRholang

}
trait AllSyntaxRholang extends RhoRuntimeSyntax
