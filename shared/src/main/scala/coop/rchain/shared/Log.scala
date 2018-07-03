package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

trait LogSource {
  val clazz: Class[_]
}

object LogSource {
  def apply(c: Class[_]) = new LogSource {
    val clazz: Class[_] = c
  }
}

trait Log[F[_]] {
  def debug(msg: String)(implicit ev: LogSource): F[Unit]
  def info(msg: String)(implicit ev: LogSource): F[Unit]
  def warn(msg: String)(implicit ev: LogSource): F[Unit]
  def error(msg: String)(implicit ev: LogSource): F[Unit]
}

object Log extends LogInstances {
  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit L: Log[F]): Log[T[F, ?]] =
    new Log[T[F, ?]] {
      def debug(msg: String)(implicit ev: LogSource): T[F, Unit] = L.debug(msg)(ev).liftM[T]
      def info(msg: String)(implicit ev: LogSource): T[F, Unit]  = L.info(msg)(ev).liftM[T]
      def warn(msg: String)(implicit ev: LogSource): T[F, Unit]  = L.warn(msg)(ev).liftM[T]
      def error(msg: String)(implicit ev: LogSource): T[F, Unit] = L.error(msg)(ev).liftM[T]
    }

  class NOPLog[F[_]: Applicative] extends Log[F] {
    def debug(msg: String)(implicit ev: LogSource): F[Unit] = ().pure[F]
    def info(msg: String)(implicit ev: LogSource): F[Unit]  = ().pure[F]
    def warn(msg: String)(implicit ev: LogSource): F[Unit]  = ().pure[F]
    def error(msg: String)(implicit ev: LogSource): F[Unit] = ().pure[F]
  }

}

sealed abstract class LogInstances {
  implicit def eitherTLog[E, F[_]: Monad: Log[?[_]]]: Log[EitherT[F, E, ?]] =
    Log.forTrans[F, EitherT[?[_], E, ?]]
}
