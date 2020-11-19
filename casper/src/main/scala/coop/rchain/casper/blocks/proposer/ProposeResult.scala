package coop.rchain.casper.blocks.proposer

import java.util.UUID

import cats.Show
import coop.rchain.casper.ValidBlock
import coop.rchain.casper.protocol.BlockMessage

final case class ProposeResult(proposeStatus: ProposeStatus)

sealed trait ProposeStatus
final case class ProposeSuccess(result: ValidBlock) extends ProposeStatus
sealed trait ProposeFailure                         extends ProposeStatus
case object InternalDeployError                     extends ProposeFailure
case object BugError                                extends ProposeFailure

sealed trait CheckProposeConstraintsResult
case object CheckProposeConstraintsSuccess extends CheckProposeConstraintsResult
sealed trait CheckProposeConstraintsFailure
    extends ProposeFailure
    with CheckProposeConstraintsResult
case object NotBonded                  extends CheckProposeConstraintsFailure
case object NotEnoughNewBlocks         extends CheckProposeConstraintsFailure
case object TooFarAheadOfLastFinalized extends CheckProposeConstraintsFailure

object CheckProposeConstraintsResult {
  def success: CheckProposeConstraintsResult = CheckProposeConstraintsSuccess
}

object ProposeResult {
  type ProposeID = UUID
  implicit val showProposeResut: Show[ProposeStatus] = new Show[ProposeStatus] {
    def show(result: ProposeStatus): String = result match {
      case ProposeSuccess(r)   => s"Propose succeed: $r"
      case NoNewDeploys        => s"Proposal failed: NoNewDeploys"
      case InternalDeployError => s"Proposal failed: internal deploy error"
      case NotBonded           => s"Proposal failed: ReadOnlyMode"
      case NotEnoughNewBlocks  => s"Proposal failed: Must wait for more blocks from other validators"
      case TooFarAheadOfLastFinalized =>
        "Proposal failed: too far ahead of the last finalized block"
      case BugError => s"Proposal failed: BugError"
    }
  }

  def noNewDeploys: ProposeFailure                   = NoNewDeploys
  def internalDeployError: ProposeFailure            = InternalDeployError
  def notBonded: ProposeResult                       = ProposeResult(NotBonded)
  def notEnoughBlocks: ProposeResult                 = ProposeResult(NotEnoughNewBlocks)
  def tooFarAheadOfLastFinalized: ProposeResult      = ProposeResult(TooFarAheadOfLastFinalized)
  def success(status: ValidBlock): ProposeResult     = ProposeResult(ProposeSuccess(status))
  def failure(status: ProposeFailure): ProposeResult = ProposeResult(status)
}

trait BlockCreatorResult
case object NoNewDeploys                             extends BlockCreatorResult with ProposeFailure
final case class Created(blockMessage: BlockMessage) extends BlockCreatorResult
object BlockCreatorResult {
  def noNewDeploys: BlockCreatorResult             = NoNewDeploys
  def created(b: BlockMessage): BlockCreatorResult = Created(b)
}
