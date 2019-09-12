package coop.rchain.casper

import cats.effect.Sync
import coop.rchain.casper.ReportingCasper.Report
import coop.rchain.models.BlockHash.BlockHash

trait ReportingCasper[F[_]] {
  def trace(hash: BlockHash): F[Report]
}

object ReportingCasper {
  sealed trait RhoEvent
  final case class DummyConsume() extends RhoEvent

  final case class DeployTrace(hash: String, events: List[RhoEvent])

  sealed trait Report
  final case class BlockNotFound(hash: String)                                extends Report
  final case class BlockTracesReport(hash: String, traces: List[DeployTrace]) extends Report

  def noop[F[_]: Sync]: ReportingCasper[F] = new ReportingCasper[F] {
    override def trace(hash: BlockHash): F[Report] =
      Sync[F].delay(BlockNotFound(hash.toStringUtf8))
  }
}
