package coop.rchain.shared

import cats.implicits._
import cats.effect.Sync
import scala.concurrent.SyncVar

class AtomicSyncVarF[F[_], A] private (private val underlying: SyncVar[A]) {
  def get(implicit sync: Sync[F]): F[A] =
    sync.delay { underlying.get }

  def modify[B](f: A => F[(A, B)])(implicit sync: Sync[F]): F[B] =
    for {
      value  <- sync.delay { underlying.take }
      fValue <- f(value).attempt
      result <- fValue match {
                 case Right((newValue, b)) =>
                   sync.delay { underlying.put(newValue) }.map(_ => b)

                 case Left(err) =>
                   sync.delay { underlying.put(value) } *> sync.raiseError(err)
               }
    } yield result

  def update(f: A => F[A])(implicit sync: Sync[F]): F[Unit] =
    modify(a => f(a).map((_, ())))
}

object AtomicSyncVarF {
  def of[F[_], A](initial: A): AtomicSyncVarF[F, A] = {
    val underlying = new SyncVar[A]()
    underlying.put(initial)
    new AtomicSyncVarF[F, A](underlying)
  }
}
