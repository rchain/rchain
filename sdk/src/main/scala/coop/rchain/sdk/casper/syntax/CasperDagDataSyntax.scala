package coop.rchain.sdk.casper.syntax

import coop.rchain.sdk.dag.data.DagData
import coop.rchain.sdk.dag.syntax._

/**
  * Casper specific extensions for DagData operations.
  */
trait CasperDagDataSyntax {
  implicit def sdkCasperSyntaxDagDataMessage[F[_], M, MId, S, SId](
      msg: M
  ): CasperDagDataMessageOps[M, MId, S, SId] = new CasperDagDataMessageOps(msg)
}

/**
  * Extensions of basic DAG types related to Casper.
  *
  * @param msg message instance
  * @tparam M represents a message
  * @tparam S represents a message sender
  */
final class CasperDagDataMessageOps[M, MId, S, SId](private val msg: M) extends AnyVal {

  /**
    * Message sender should be present and have non zero stake in the bonds map.
    */
  def inactiveSender(implicit dagData: DagData[M, MId, S, SId]): Boolean =
    !msg.bondsMap.exists { case (s, stake) => msg.sender == s && stake > 0 }

  /**
    * Message should have sequence number equal to sequence number of self justification + 1.
    */
  def checkSequenceNumber(
      selfJustification: M
  )(implicit dagData: DagData[M, MId, S, SId]): Boolean =
    msg.seqNum != selfJustification.seqNum + 1
}
