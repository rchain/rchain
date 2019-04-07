package coop.rchain.catscontrib.mtl

import cats.{Functor, MonadError}
import cats.mtl.FunctorRaise
import cats.mtl.implicits._

package object implicits {

  // Derive _error[Task] = FunctorRaise[Task, InterpreterError] and similar
  // based on their MonadError[_, Throwable] instance
  implicit def monadErrorFunctorRaise[F[_], E <: Throwable](
      implicit monadError: MonadError[F, Throwable]
  ): FunctorRaise[F, E] = new FunctorRaise[F, E] {
    override val functor: Functor[F]  = monadError
    override def raise[A](e: E): F[A] = monadError.raiseError(e)
  }

}
