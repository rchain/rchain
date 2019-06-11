package coop.rchain.rholang.interpreter.error

import cats.effect.{Bracket, Concurrent}
import cats.implicits._
import cats.effect.concurrent.MVar

final class MLock[F[_]](lock: MVar[F, Unit])(implicit bracketF: Bracket[F, Throwable]) {

  def acquire: F[Unit] = lock.take

  /**
    * Attempts to release lock.
    *
    * Throws an error if lock is already released.
    *
    */
  def release: F[Unit] =
    lock
      .tryPut(())
      .ifM(
        ().pure[F],
        LockReleaseException("attempted to release released lock").raiseError[F, Unit]
      )

  def withLock[A](fa: F[A]): F[A] = bracketF.bracket[Unit, A](acquire)(_ => fa)(_ => release)

  /**
    * Blocks asynchronously until lock is released.
    *
    * The next call to release will unblock all readers simultaneously.
    *
    */
  def waitForLock: F[Unit] = lock.read

}

object MLock {

  def apply[F[_]: Concurrent]: F[MLock[F]] = MVar.of[F, Unit](()).map(new MLock[F](_))

}
