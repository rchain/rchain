package coop.rchain.shared

import cats._
import cats.implicits._

sealed abstract class StreamT[F[_], +A] { self =>

  def ++[AA >: A](other: StreamT[F, AA]): StreamT[F, A] = ???

  def map[B](f: A => B)(implicit functor: Functor[F]): StreamT[F, B] = self match {
    case Cons(head, tail) => StreamT.cons(f(head), tail.map(_.map(_.map(f))))
    case _: Nil[F]        => StreamT.empty[F, B]
  }

  def flatMap[B](f: A => StreamT[F, B])(implicit monad: Monad[F]): StreamT[F, B] = self match {
    case Cons(head, lazyTail) =>
      f(head) match {
        case Cons(newHead, mappedLazyTail) =>
          val newLazyTail: Eval[F[StreamT[F, B]]] = for {
            tailF       <- lazyTail
            mappedTailF <- mappedLazyTail
          } yield
            for {
              mappedTail <- mappedTailF
              tail       <- tailF
            } yield mappedTail ++ tail.flatMap[B](f)
          StreamT.cons(newHead, newLazyTail)

        case _: Nil[F] => StreamT.skip(lazyTail.map(_.map(_.flatMap(f))))
      }
    case _: Nil[F] => StreamT.empty[F, B]
  }

  def foreach(f: A => F[Unit])(implicit monad: Monad[F]): F[Unit] = self match {
    case Cons(head, tailF) =>
      for {
        _    <- f(head)
        tail <- tailF.value
        _    <- tail.foreach(f)
      } yield ()

    case _: Nil[F] => ().pure[F]
  }

  def take(n: Int)(implicit functor: Functor[F]): StreamT[F, A] =
    if (n > 0) self match {
      case Cons(head, tail) => StreamT.cons(head, tail.map(_.map(_.take(n - 1))))
      case _: Nil[F]        => StreamT.empty[F, A]
    } else StreamT.empty[F, A]
}
final case class Cons[F[_], A](head: A, tail: Eval[F[StreamT[F, A]]]) extends StreamT[F, A]
final case class Skip[F[_], A](tail: Eval[F[StreamT[F, A]]])          extends StreamT[F, A]
final class Nil[F[_]]                                                 extends StreamT[F, Nothing]

object StreamT {
  def empty[F[_], A]: StreamT[F, A]                                       = new Nil[F]
  def cons[F[_], A](head: A, tail: Eval[F[StreamT[F, A]]]): StreamT[F, A] = Cons(head, tail)
  def skip[F[_], A](tail: Eval[F[StreamT[F, A]]]): StreamT[F, A]          = Skip(tail)
}
