package coop.rchain.rholang

import cats.implicits._
import cats.Monad
import cats.effect.{Bracket, Concurrent}
import cats.effect.concurrent.Ref
import cats.mtl.{DefaultMonadState, MonadState}

package object interpreter {

  type _bracket[F[_]] = Bracket[F, Throwable]

  def _bracket[F[_]](implicit ev: _bracket[F]): _bracket[F] = ev

  def of[F[_]: Concurrent, A](init: A): F[MonadState[F, A]] =
    Ref[F]
      .of(init)
      .map(defaultMonadState)

  def defaultMonadState[F[_]: Monad: Concurrent, A] =
    (state: Ref[F, A]) =>
      new DefaultMonadState[F, A] {
        val monad: cats.Monad[F] = implicitly[Monad[F]]
        def get: F[A]            = state.get
        def set(s: A): F[Unit]   = state.set(s)

        override def modify(f: A => A): F[Unit] = state.modify(f.map((_, ())))
      }

}
