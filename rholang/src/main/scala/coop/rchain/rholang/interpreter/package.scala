package coop.rchain.rholang

import coop.rchain.rholang.interpreter.errors.InterpreterError
import cats.mtl.FunctorRaise

package object interpreter {

  type _error[F[_]] = FunctorRaise[F, InterpreterError]

  def _error[F[_]](implicit ev: _error[F]): _error[F] = ev
}
