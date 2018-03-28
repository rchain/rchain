package coop.rchain.rosette.utils

import cats.data.StateT
import cats.{Applicative, MonadError}

package object implicits {
  implicit class LiftOps[E, A, S](e: E) {
    def liftE[F[_]: Applicative, EE >: E](implicit E: MonadError[F, EE]): StateT[F, S, A] =
      StateT.liftF[F, S, A](E.raiseError(e))
  }
}
