package coop.rchain.shared

import java.util.concurrent.atomic.AtomicReference

import cats.Monad
import cats.effect.Sync
import cats.mtl.MonadState
import cats.implicits._

import scala.annotation.tailrec

//Inspired by Ref from  fs2 and cats
//TODO: Remove once we upgrade to cats 1.0.1
final class AtomicRefMonadState[F[_], S] private (private val ar: AtomicReference[S])(
    implicit F: Sync[F])
    extends MonadState[F, S] {

  override def set(s: S): F[Unit] = F.delay(ar.set(s))

  override def inspect[A](f: S => A): F[A] = get.map(f)

  override val monad: Monad[F] = implicitly

  override def get: F[S] = F.delay(ar.get)

  override def modify(f: S => S): F[Unit] = {
    @tailrec
    def spin(): Unit = {
      val c = ar.get
      val u = f(c)
      if (!ar.compareAndSet(c, u)) spin()
      else ()
    }
    F.delay(spin)
  }
}

object AtomicRefMonadState {
  def of[F[_]: Sync, S](init: S): AtomicRefMonadState[F, S] =
    new AtomicRefMonadState[F, S](new AtomicReference[S](init))
}
