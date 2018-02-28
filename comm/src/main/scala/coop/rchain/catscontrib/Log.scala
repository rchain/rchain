package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Log[F[_]] {
  def info(msg: String): F[Unit]
  def warn(msg: String): F[Unit]
  def error(msg: String): F[Unit]
}

object Log extends LogInstances {
  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit L: Log[F]): Log[T[F, ?]] =
    new Log[T[F, ?]] {

      def info(msg: String): T[F, Unit]  = L.info(msg).liftM[T]
      def warn(msg: String): T[F, Unit]  = L.warn(msg).liftM[T]
      def error(msg: String): T[F, Unit] = L.error(msg).liftM[T]
    }
}

sealed abstract class LogInstances {

  implicit def eitherTLog[E, F[_]: Monad: Log[?[_]]]: Log[EitherT[F, E, ?]] =
    Log.forTrans[F, EitherT[?[_], E, ?]]

  implicit def writerTKvs[W: Monoid, F[_]: Monad: Log[?[_]]]: Log[WriterT[F, W, ?]] =
    Log.forTrans[F, WriterT[?[_], W, ?]]
}
