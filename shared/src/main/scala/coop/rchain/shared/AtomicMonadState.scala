package coop.rchain.shared

import cats._
import cats.mtl.MonadState
import cats.effect._

import monix.execution.atomic.Atomic

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While")) // false positive
class AtomicMonadState[F[_]: Sync, S](state: Atomic[S])(implicit val monad: Monad[F])
    extends MonadState[F, S] {
  def get: F[S]                   = Sync[F].delay(state.get)
  def set(s: S): F[Unit]          = Sync[F].delay(state.set(s))
  def inspect[A](f: S => A): F[A] = Sync[F].delay(f(state.get))
  def modify(f: S => S): F[Unit]  = Sync[F].delay(state.transform(f))
}
