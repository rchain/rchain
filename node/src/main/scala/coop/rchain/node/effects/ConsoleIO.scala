package coop.rchain.node.effects

import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._

trait ConsoleIO[F[_]] {
  def readLine: F[String]
  def println(str: String): F[Unit]
  def updateCompletion(history: Set[String]): F[Unit]
  def close: F[Unit]
}

object ConsoleIO extends ConsoleIO0 {
  def apply[F[_]](implicit ev: ConsoleIO[F]): ConsoleIO[F] = ev
}

trait ConsoleIO0 {
  import eitherT._
  implicit def eitherTConsoleIO[F[_]: Monad: ConsoleIO, E]: ConsoleIO[EitherT[F, E, ?]] =
    ForTrans.forTrans[F, EitherT[?[_], E, ?]]
}

class NOPConsoleIO[F[_]: Applicative] extends ConsoleIO[F] {
  def readLine: F[String]                             = "".pure[F]
  def println(str: String): F[Unit]                   = ().pure[F]
  def updateCompletion(history: Set[String]): F[Unit] = ().pure[F]
  def close: F[Unit]                                  = ().pure[F]
}

object ForTrans {
  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit C: ConsoleIO[F]): ConsoleIO[T[F, ?]] =
    new ConsoleIO[T[F, ?]] {
      def readLine: T[F, String]           = MonadTrans[T].liftM(ConsoleIO[F].readLine)
      def println(str: String): T[F, Unit] = MonadTrans[T].liftM(ConsoleIO[F].println(str))
      def updateCompletion(history: Set[String]): T[F, Unit] =
        MonadTrans[T].liftM(ConsoleIO[F].updateCompletion(history))
      def close: T[F, Unit] = MonadTrans[T].liftM(ConsoleIO[F].close)
    }
}
