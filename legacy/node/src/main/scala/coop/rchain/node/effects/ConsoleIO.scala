package coop.rchain.node.effects

import cats._
import cats.implicits._
import coop.rchain.shared.StringOps.ColoredString

trait ConsoleIO[F[_]] {
  def readLine: F[String]
  def readPassword(prompt: String): F[String]
  def println(str: String): F[Unit]
  def println(str: ColoredString): F[Unit]
  def updateCompletion(history: Set[String]): F[Unit]
  def close: F[Unit]
}

object ConsoleIO {
  def apply[F[_]](implicit ev: ConsoleIO[F]): ConsoleIO[F] = ev
}

class NOPConsoleIO[F[_]: Applicative] extends ConsoleIO[F] {
  def readLine: F[String]                             = "".pure[F]
  def readPassword(prompt: String): F[String]         = "".pure[F]
  def println(str: String): F[Unit]                   = ().pure[F]
  def updateCompletion(history: Set[String]): F[Unit] = ().pure[F]
  def close: F[Unit]                                  = ().pure[F]
  def println(str: ColoredString): F[Unit]            = ().pure[F]
}
