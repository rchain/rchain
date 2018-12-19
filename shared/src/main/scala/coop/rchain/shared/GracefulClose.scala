package coop.rchain.shared

import java.nio.file._
import cats._, cats.data._, cats.implicits._
import cats.effect.Sync

object GracefulClose {
  def gracefullyClose[F[_]: Sync](closable: AutoCloseable): F[Either[Throwable, Unit]] =
    Sync[F].delay(closable.close).attempt
}
