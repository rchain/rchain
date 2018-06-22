package coop.rchain.shared

import cats._
import cats.mtl.MonadState

import coop.rchain.catscontrib.Capture

import monix.execution.atomic.Atomic

class AtomicMonadState[F[_]: Capture, S](state: Atomic[S])(implicit val monad: Monad[F])
    extends MonadState[F, S] {
  def get: F[S]                   = Capture[F].capture(state.get)
  def set(s: S): F[Unit]          = Capture[F].capture(state.set(s))
  def inspect[A](f: S => A): F[A] = Capture[F].capture(f(state.get))
  def modify(f: S => S): F[Unit]  = Capture[F].capture(state.transform(f))
}
