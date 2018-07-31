package coop.rchain.rholang.interpreter.matcher
import cats.{Applicative, Functor, Monad}

import scala.collection.immutable.Stream
import cats.implicits._

final case class StreamT[F[_], A](value: F[Stream[A]]) {
  def ap[B](f: F[A => B])(implicit F: Monad[F]): StreamT[F, B] =
    StreamT(F.flatMap(f)(ff => F.map(value)((stream: Stream[A]) => stream.map(ff))))

  def map[B](f: A => B)(implicit F: Functor[F]): StreamT[F, B] =
    StreamT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => StreamT[F, B])(implicit F: Monad[F]): StreamT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Stream[B]])(implicit F: Monad[F]): StreamT[F, B] =
    StreamT(Monad[F].flatMap(value)(astream => astream.flatTraverse(a => f(a))))
}

object StreamT extends StreamTInstances0 {
  def pure[F[_], A](single: A)(implicit F: Applicative[F]): StreamT[F, A] =
    StreamT(F.pure(Stream(single)))
}

trait StreamTInstances0 {
  implicit def streamTMonad[F[_]: Monad]: Monad[StreamT[F, ?]] = new Monad[StreamT[F, ?]] {
    override def pure[A](x: A): StreamT[F, A] = StreamT(Monad[F].pure(Stream(x)))

    override def flatMap[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => StreamT[F, Either[A, B]]): StreamT[F, B] =
      f(a).flatMap {
        case Right(b) => StreamT.pure(b)
        case Left(e)  => tailRecM(e)(f)
      }
  }
}
