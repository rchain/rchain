package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.{~>, Applicative, Functor, Monad}
import cats.mtl.{ApplicativeAsk, FunctorTell, MonadState}

package object error {

  type _error[F[_]] = ApplicativeAsk[F, Option[Throwable]] with FunctorTell[F, Throwable]

  def _error[F[_]](implicit ev: _error[F]): _error[F] = ev

  def errorHandler[F[_]: Monad](
      errorRef: MonadState[F, Option[Throwable]],
      errorLock: RWLock[F]
  ): _error[F] = new ApplicativeAsk[F, Option[Throwable]] with FunctorTell[F, Throwable] {

    val applicative: Applicative[F]                = Applicative[F]
    def ask: F[Option[Throwable]]                  = errorLock.withReadLock(errorRef.get)
    def reader[A](f: Option[Throwable] => A): F[A] = errorLock.withReadLock(errorRef.inspect(f))

    val functor: Functor[F]                 = Functor[F]
    def tell(l: Throwable): F[Unit]         = errorLock.withWriteLock(errorRef.set(l.some))
    def writer[A](a: A, l: Throwable): F[A] = tell(l) *> a.pure[F]
    def tuple[A](ta: (Throwable, A)): F[A]  = writer(ta._2, ta._1)
  }

  implicit def ntErrorHandler[F[_], G[_]: Monad](nt: F ~> G)(implicit error: _error[F]): _error[G] =
    new ApplicativeAsk[G, Option[Throwable]] with FunctorTell[G, Throwable] {

      val applicative: Applicative[G]                = Applicative[G]
      def ask: G[Option[Throwable]]                  = nt(error.ask)
      def reader[A](f: Option[Throwable] => A): G[A] = nt(error.reader(f))

      val functor: Functor[G]                 = Functor[G]
      def tell(l: Throwable): G[Unit]         = nt(error.tell(l))
      def writer[A](a: A, l: Throwable): G[A] = nt(error.writer(a, l))
      def tuple[A](ta: (Throwable, A)): G[A]  = nt(error.tuple(ta))
    }
}
