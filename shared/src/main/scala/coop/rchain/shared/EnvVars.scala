package coop.rchain.shared

import cats.effect.Sync

trait EnvVars[F[_]] {
  def get(key: String): F[Option[String]]
}

object EnvVars {
  def apply[F[_]](implicit ev: EnvVars[F]): EnvVars[F] = ev

  def envVars[F[_]: Sync]: EnvVars[F] = (key: String) => Sync[F].delay { sys.env.get(key) }
}
