package coop.rchain.shared

import cats._
import cats.implicits._
import cats.mtl.MonadState

import monix.execution.atomic.Atomic

class AtomicMonadState[F[_], S](state: Atomic[S])(implicit val monad: Monad[F])
    extends MonadState[F, S] {
  def get: F[S]                   = state.get.pure[F]
  def set(s: S): F[Unit]          = state.set(s).pure[F]
  def inspect[A](f: S => A): F[A] = f(state.get).pure[F]
  def modify(f: S => S): F[Unit]  = state.transform(f).pure[F]
}
