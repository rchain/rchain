package coop.rchain.rosette.utils

import cats.data.Kleisli
import cats.MonadError

package object implicits {
  implicit class LiftOps[E, A, B, F[_]](e: E) {
    def liftE[EE >: E](implicit E: MonadError[F, EE]): Kleisli[F, A, B] =
      Kleisli.liftF[F, A, B](E.raiseError(e))
  }
}
