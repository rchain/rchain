package coop.rchain.rholang.interpreter.accounting

import cats.effect.Concurrent
import cats.effect.concurrent.MVar
import cats.implicits._
import cats.mtl._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

object CostAccounting {

  def of[F[_]: Concurrent](init: Cost): F[MonadState[F, Cost]] =
    MVar[F]
      .of(init)
      .map(defaultMonadState)

  def empty[F[_]: Concurrent]: F[MonadState[F, Cost]] =
    MVar[F]
      .empty[Cost]
      .map(defaultMonadState)

  private[this] def defaultMonadState[F[_]: Monad] =
    (state: MVar[F, Cost]) =>
      new DefaultMonadState[F, Cost] {
        val monad: cats.Monad[F]                    = implicitly[Monad[F]]
        def get: F[Cost]                            = state.take
        def set(s: Cost): F[Unit]                   = state.put(s)
        override def inspect[A](f: Cost => A): F[A] = state.read.map(f)
        override def modify(f: Cost => Cost): F[Unit] =
          for {
            s <- get
            _ <- set(f(s))
          } yield ()

      }
}
