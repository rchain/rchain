package coop.rchain.shared

import cats._
import cats.implicits._
import cats.mtl.MonadState

import monix.catnap.MVar
import monix.eval.Task

class MVarMonadState[S](state: Task[MVar[Task, S]])(implicit val monad: Monad[Task])
    extends MonadState[Task, S] {
  /*
    Removes a value from the state.
    Blocks if the state is empty.
   */
  def get: Task[S] = state >>= (_.take)

  /*
    Sets a value in the state
    Blocks if the state is non-empty
   */
  def set(s: S): Task[Unit] = state >>= (_.put(s))

  /*
    Don't use inspect for a read & write access pattern.
    Don't use inspect & set or inspect & modify
    because the state may have changed in the meantime
   */
  def inspect[A](f: S => A): Task[A] = state >>= (_.read.map(f))

  /*
    Use with care. You probably should use set instead.
    Don't use get & modify together or it may block until someone calls set.
    Don't use inspect & modify because the state may have changed in the meantime
   */
  def modify(f: S => S): Task[Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}
