package coop.rchain.rholang.interpreter.accounting

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl._
import cats.{FlatMap, Monad}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

object CostAccounting {

  def of[F[_]: Sync](init: Cost): F[MonadState[F, Cost]] =
    Ref[F]
      .of(init)
      .map(
        state =>
          new DefaultMonadState[F, Cost] {
            val monad: cats.Monad[F]                      = implicitly[Monad[F]]
            def get: F[Cost]                              = state.get
            def set(s: Cost): F[Unit]                     = state.set(s)
            override def modify(f: Cost => Cost): F[Unit] = state.update(f)
          }
      )

  def empty[F[_]: Sync]: F[MonadState[F, Cost]] =
    this.of(Cost(0, "init"))

}
