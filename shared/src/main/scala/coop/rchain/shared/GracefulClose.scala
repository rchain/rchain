package coop.rchain.shared

import cats.effect.Sync
import cats.implicits._

object GracefulClose {
  def gracefullyClose[F[_]: Sync](closable: AutoCloseable): F[Either[Throwable, Unit]] =
    Sync[F].delay(closable.close()).attempt
}
