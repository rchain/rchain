package coop.rchain.shared

import cats._
import cats.data._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import coop.rchain.catscontrib.effect.implicits._

trait LogSource {
  val clazz: Class[_]
}

object LogSource {
  def apply(c: Class[_]): LogSource = new LogSource {
    val clazz: Class[_] = c
  }

  implicit def matLogSource: LogSource = macro LogSourceMacros.mkLogSource
}

class LogSourceMacros(val c: blackbox.Context) {
  import c.universe._

  def mkLogSource: c.Expr[LogSource] = {
    val tree =
      q"""
          coop.rchain.shared.LogSource(${c.reifyEnclosingRuntimeClass}.asInstanceOf[Class[_]])
       """

    c.Expr[LogSource](tree)
  }
}

trait Log[F[_]] {
  def isTraceEnabled(implicit ev: LogSource): F[Boolean]
  def trace(msg: String)(implicit ev: LogSource): F[Unit]
  def debug(msg: String)(implicit ev: LogSource): F[Unit]
  def info(msg: String)(implicit ev: LogSource): F[Unit]
  def warn(msg: String)(implicit ev: LogSource): F[Unit]
  def error(msg: String)(implicit ev: LogSource): F[Unit]
  def error(msg: String, cause: Throwable)(implicit ev: LogSource): F[Unit]
}

object Log extends LogInstances {
  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit L: Log[F]): Log[T[F, ?]] =
    new Log[T[F, ?]] {
      def isTraceEnabled(implicit ev: LogSource): T[F, Boolean]  = L.isTraceEnabled.liftM[T]
      def trace(msg: String)(implicit ev: LogSource): T[F, Unit] = L.trace(msg)(ev).liftM[T]
      def debug(msg: String)(implicit ev: LogSource): T[F, Unit] = L.debug(msg)(ev).liftM[T]
      def info(msg: String)(implicit ev: LogSource): T[F, Unit]  = L.info(msg)(ev).liftM[T]
      def warn(msg: String)(implicit ev: LogSource): T[F, Unit]  = L.warn(msg)(ev).liftM[T]
      def error(msg: String)(implicit ev: LogSource): T[F, Unit] = L.error(msg)(ev).liftM[T]
      def error(msg: String, cause: Throwable)(implicit ev: LogSource): T[F, Unit] =
        L.error(msg, cause)(ev).liftM[T]
    }

  class NOPLog[F[_]: Applicative] extends Log[F] {
    def isTraceEnabled(implicit ev: LogSource): F[Boolean]                    = false.pure[F]
    def trace(msg: String)(implicit ev: LogSource): F[Unit]                   = ().pure[F]
    def debug(msg: String)(implicit ev: LogSource): F[Unit]                   = ().pure[F]
    def info(msg: String)(implicit ev: LogSource): F[Unit]                    = ().pure[F]
    def warn(msg: String)(implicit ev: LogSource): F[Unit]                    = ().pure[F]
    def error(msg: String)(implicit ev: LogSource): F[Unit]                   = ().pure[F]
    def error(msg: String, cause: Throwable)(implicit ev: LogSource): F[Unit] = ().pure[F]
  }

}

sealed abstract class LogInstances {
  implicit def eitherTLog[E, F[_]: Monad: Log[?[_]]]: Log[EitherT[F, E, ?]] =
    Log.forTrans[F, EitherT[?[_], E, ?]]

  def log[F[_]: Sync]: Log[F] = new Log[F] {
    import com.typesafe.scalalogging.Logger

    def isTraceEnabled(implicit ev: LogSource): F[Boolean] =
      Sync[F].delay(Logger(ev.clazz).underlying.isTraceEnabled())
    def trace(msg: String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).trace(msg))
    def debug(msg: String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).debug(msg))
    def info(msg: String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).info(msg))
    def warn(msg: String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).warn(msg))
    def error(msg: String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).error(msg))
    def error(msg: String, cause: Throwable)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(Logger(ev.clazz).error(msg, cause))
  }

  val logId: Log[Id] = log
}
