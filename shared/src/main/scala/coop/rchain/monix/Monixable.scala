package coop.rchain.monix

import cats.data.ReaderT
import cats.~>
import monix.eval.Task

/**
  * The purpose of this interface is to be a bridge to abstract effect type in old code which
  *  uses monix Task directly.
  */
trait Monixable[F[_]] {
  def toTask[A](t: F[A]): Task[A]

  def fromTask[A](t: Task[A]): F[A]
}

object Monixable {
  def apply[F[_]](implicit instance: Monixable[F]): Monixable[F] = instance

  // Default implementation is just the identity for monix Task
  implicit object MonixableTask extends Monixable[Task] {
    override def toTask[A](task: Task[A]): Task[A] = task

    override def fromTask[A](task: Task[A]): Task[A] = task
  }

  // FunctorK (specific to ReaderT)
  implicit class MonixMapKOps[F[_]](val m: Monixable[F]) extends AnyVal {
    def mapK[S](nt: F ~> ReaderT[F, S, *], s: S): Monixable[ReaderT[F, S, *]] =
      new Monixable[ReaderT[F, S, *]] {
        override def toTask[A](t: ReaderT[F, S, A]): Task[A]   = m.toTask(t.run(s))
        override def fromTask[A](t: Task[A]): ReaderT[F, S, A] = nt(m.fromTask(t))
      }
  }
}
