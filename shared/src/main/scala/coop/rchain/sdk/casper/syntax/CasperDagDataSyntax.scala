package coop.rchain.sdk.casper.syntax

import coop.rchain.sdk.dag.data.DagData
import coop.rchain.sdk.dag.syntax._

trait CasperDagDataSyntax {
  implicit def sdkCasperSyntaxDagDataMessage[F[_], M, S](m: M): CasperDagDataMessageOps[M, S] =
    new CasperDagDataMessageOps(m)
}

sealed trait MalformedMessageReason

/**
  * Extensions of basic DAG types related to Casper.
  *
  * @param m message instance
  * @tparam M represents a message
  * @tparam S represents a message sender
  */
final class CasperDagDataMessageOps[M, S](private val m: M) extends AnyVal {

  /**
    * Message should be well formed.
    *
    * If sender creates and signs message that contains junk data, there are two options: just ignore, or act.
    * Just ignoring introduces DoS attack vector when lots of blocks have to be verified, because the whole
    * message verification takes more resources then just verifying the signature.
    * Therefore ill formed message is an offence.
    *
    * TODO: If this function needs other data then M it should be moved to [[DagViewSyntax]] syntax.
    */
  def invalidMalformedMessage(checkFormat: M => MalformedMessageReason): MalformedMessageReason =
    checkFormat(m)

  /**
    * Message sender should be present and have non zero stake in map.
    */
  def inactiveSender(implicit dagData: DagData[M, S]): Boolean =
    !m.bondsMap.exists { case (s, stake) => m.sender == s && stake > 0 }

  /**
    * Message should have justification for each senders in the bonds map (both active and inactive).
    */
  def invalidJustificationFollows(bondedSenders: Set[S])(implicit dagData: DagData[M, S]): Boolean =
    m.justifications.map(_.sender) != bondedSenders

  /**
    * Message should have sequence number equal to sequence number of self justification + 1.
    * [[incompleteJustificationSet]] guarantees that self justification s present.
    */
  def invalidSequenceNumber(selfJustification: M)(implicit dagData: DagData[M, S]): Boolean =
    m.seqNum != selfJustification.seqNum + 1

  /**
    * Message should have block number equal to the block number of the highest justification + 1.
    */
  def invalidBlockNumber(implicit dagData: DagData[M, S]): Boolean =
    m.blockNum != m.justifications.map(_.blockNum).max + 1

  /**
    * Message should not have justifications past to justification of previous message from the same sender.
    */
  def invalidJustificationRegression(implicit dagData: DagData[M, S]): Boolean = {
    val js       = m.justifications
    val selfJOpt = js.find(_.sender == m.sender)
    assert(selfJOpt.isDefined, "Message does not have self justification.")
    val selfJ   = selfJOpt.get
    val selfJjs = selfJ.justifications
    js.toIterator.map { j =>
      selfJjs.exists { selfJj =>
        // Previous justification from the same sender has seq num higher then the current one.
        selfJj.sender == j.sender && selfJj.seqNum > j.seqNum
      }
    }.nonEmpty
  }
}
