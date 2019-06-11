package coop.rchain.rholang.interpreter.error

import cats.effect.concurrent.MVar
import cats.effect.{Bracket, Concurrent}
import cats.implicits._

/**
  * [[Free]]: Read or write lock can be acquired without blocking.
  *
  * [[Write]]: Read and write locks block until lock is released.
  *
  * [[Read]]: Write locks block until all read locks are released.
  *
  * @note Nesting the monadic combinators is not advised as each
  *       combination either dead-locks or throws an error.
  *
  */
sealed trait State
case object Write                 extends State
case object Free                  extends State
final case class Read(count: Int) extends State

final class RWLock[F[_]](stateRef: MVar[F, State], readLock: MLock[F], writeLock: MLock[F])(
    implicit bracketF: Bracket[F, Throwable]
) {

  /**
    * Acquires read lock.
    *
    * Blocks (asynchronously) if another thread has write lock.
    *
    */
  def acquireRead: F[Unit] =
    stateRef.take >>= {
      case Free        => readLock.acquire >> stateRef.put(Read(1))
      case Write       => stateRef.put(Write) >> writeLock.waitForLock >> acquireRead
      case Read(count) => stateRef.put(Read(count + 1))
    }

  /**
    * Attempts to acquire read lock.
    *
    * Non-blocking.
    *
    * @return True if read lock was acquired. False otherwise.
    *
    */
  def tryAcquireRead: F[Boolean] =
    stateRef.take >>= {
      case Free        => readLock.acquire >> stateRef.put(Read(1)) >> true.pure[F]
      case Write       => stateRef.put(Write) >> false.pure[F]
      case Read(count) => stateRef.put(Read(count - 1)) >> true.pure[F]
    }

  /**
    * Releases read lock.
    *
    */
  def releaseRead: F[Unit] =
    stateRef.take >>= {
      case Read(1)     => readLock.release >> stateRef.put(Free)
      case Read(count) => readLock.release >> stateRef.put(Read(count - 1))
      case state =>
        stateRef.put(state) >> LockReleaseException("attempted to release released read lock")
          .raiseError[F, Unit]
    }

  /**
    * Performs a computation with read lock acquired.
    *
    * Blocks (asynchronously) if another thread has write lock.
    *
    * Read lock is guaranteed to be released whether or not computation succeeds.
    *
    */
  def withReadLock[A](fa: F[A]): F[A] = bracketF.bracket(acquireRead)(_ => fa)(_ => releaseRead)

  /**
    * Attempts to perform a computation with read lock acquired.
    *
    * Non-blocking.
    *
    * Read lock is guaranteed to be released whether or not computation succeeds.
    *
    */
  def tryWithRead[A](fa: F[A]): F[Option[A]] =
    tryAcquireRead >>= {
      case true  => bracketF.guarantee(fa)(releaseRead).map(_.some)
      case false => none[A].pure[F]
    }

  /**
    * Acquires write lock.
    *
    * Blocks (asynchronously) if other threads have read or write locks.
    *
    */
  def acquireWrite: F[Unit] =
    stateRef.take >>= {
      case Free            => writeLock.acquire >> stateRef.put(Write)
      case Write           => stateRef.put(Write) >> writeLock.waitForLock >> acquireWrite
      case state @ Read(_) => stateRef.put(state) >> readLock.waitForLock >> acquireWrite
    }

  /**
    * Attempts to acquire write lock.
    *
    * Non-blocking.
    *
    * @return True if write lock was acquired. False otherwise.
    *
    */
  def tryAcquireWrite: F[Boolean] =
    stateRef.take >>= {
      case Free  => writeLock.acquire >> stateRef.put(Write) >> true.pure[F]
      case state => stateRef.put(state) >> false.pure[F]
    }

  /**
    * Releases write lock.
    *
    */
  def releaseWrite: F[Unit] =
    stateRef.take >>= {
      case Write =>
        writeLock.release >> stateRef.put(Free)
      case state =>
        stateRef.put(state) >> LockReleaseException("attempted to release released write lock")
          .raiseError[F, Unit]
    }

  /**
    * Performs a computation with write lock acquired.
    *
    * Blocks (asynchronously) if other threads have read or write locks.
    *
    * Write lock is guaranteed to be released whether or not computation succeeds.
    *
    */
  def withWriteLock[A](fa: F[A]): F[A] = bracketF.bracket(acquireWrite)(_ => fa)(_ => releaseWrite)

  /**
    * Attempts to perform a computation with write lock acquired.
    *
    * Non-blocking.
    *
    * Write lock is guaranteed to be released whether or not computation succeeds.
    *
    */
  def tryWithWrite[A](fa: F[A]): F[Option[A]] =
    tryAcquireWrite >>= {
      case true  => bracketF.guarantee(fa)(releaseWrite).map(_.some)
      case false => none[A].pure[F]
    }
}

object RWLock {

  def apply[F[_]: Concurrent]: F[RWLock[F]] =
    for {
      stateRef  <- MVar.of[F, State](Free)
      readLock  <- MLock[F]
      writeLock <- MLock[F]
    } yield new RWLock(stateRef, readLock, writeLock)

}
