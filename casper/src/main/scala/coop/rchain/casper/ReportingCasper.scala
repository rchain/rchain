package coop.rchain.casper

import cats.effect.Sync
import coop.rchain.models.BlockHash.BlockHash

trait ReportingCasper[F[_]] {
  def trace(hash: BlockHash): F[String]
}

object ReportingCasper {
  def noop[F[_]: Sync]: ReportingCasper[F] = new ReportingCasper[F] {
    override def trace(hash: BlockHash): F[String] =
      Sync[F].delay(hash.toStringUtf8)
  }
}
