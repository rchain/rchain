package coop.rchain.sdk.dag.syntax

import coop.rchain.sdk.dag.data.DagData
import fs2.Stream

trait DagDataSyntax {
  implicit def sdkDagSyntaxDagDataMessage[F[_], M, S](m: M): DagDataMessageOps[M, S] =
    new DagDataMessageOps(m)

  implicit def sdkDagSyntaxDagDataSender[F[_], M, S](s: S): DagDataSenderOps[M, S] =
    new DagDataSenderOps(s)
}

final class DagDataMessageOps[M, S](private val m: M) extends AnyVal {
  /* Boilerplate code to enable field like access to M type */
  def seqNum(implicit dagData: DagData[M, S]): Long           = dagData.seqNum(m)
  def blockNum(implicit dagData: DagData[M, S]): Long         = dagData.blockNum(m)
  def justifications(implicit dagData: DagData[M, S]): Set[M] = dagData.justifications(m)
  def sender(implicit dagData: DagData[M, S]): S              = dagData.sender(m)
  def bondsMap(implicit dagData: DagData[M, S]): Map[S, Long] = dagData.bondsMap(m)

  /**
    * Stream of self justifications.
    */
  def selfJustificationChain[F[_]](implicit dagData: DagData[M, S]): Stream[F, M] =
    Stream.unfold(m)(_.justifications.find(_.sender == m.sender).map(v => (v, v)))
}

final class DagDataSenderOps[M, S](private val s: S) extends AnyVal {
  /* Boilerplate code to enable field like access to S type */
  def stake(implicit dagData: DagData[M, S]): Long = dagData.stake(s)
}
