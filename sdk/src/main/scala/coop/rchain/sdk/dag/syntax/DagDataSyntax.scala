package coop.rchain.sdk.dag.syntax

import coop.rchain.sdk.dag.data.DagData

trait DagDataSyntax {
  implicit def sdkDagSyntaxDagDataMessage[F[_], M, MId, S, SId](
      m: M
  ): DagDataMessageOps[M, MId, S, SId] =
    new DagDataMessageOps(m)

  implicit def sdkDagSyntaxDagDataSender[F[_], M, MId, S, SId](
      s: S
  ): DagDataSenderOps[M, MId, S, SId] =
    new DagDataSenderOps(s)
}

final class DagDataMessageOps[M, MId, S, SId](private val m: M) extends AnyVal {
  /* Boilerplate code to enable field like access to M type */
  def seqNum(implicit dagData: DagData[M, MId, S, SId]): Long   = dagData.seqNum(m)
  def blockNum(implicit dagData: DagData[M, MId, S, SId]): Long = dagData.blockNum(m)
  def justifications(implicit dagData: DagData[M, MId, S, SId]): Set[MId] =
    dagData.justifications(m)
  def sender(implicit dagData: DagData[M, MId, S, SId]): SId              = dagData.sender(m)
  def bondsMap(implicit dagData: DagData[M, MId, S, SId]): Map[SId, Long] = dagData.bondsMap(m)
}

final class DagDataSenderOps[M, MId, S, SId](private val s: S) extends AnyVal {
  /* Boilerplate code to enable field like access to S type */
  def stake(implicit dagData: DagData[M, MId, S, SId]): Long = dagData.stake(s)
}
