package coop.rchain.casper

import coop.rchain.casper.protocol.BlockMessage

sealed trait CreateBlockStatus {
  def map(f: BlockMessage.Safe => BlockMessage.Safe): CreateBlockStatus =
    this
}
sealed trait NoBlock extends CreateBlockStatus
case class Created(block: BlockMessage.Safe) extends CreateBlockStatus {
  override def map(f: BlockMessage.Safe => BlockMessage.Safe): CreateBlockStatus =
    Created(f(block))
}
case class InternalDeployError(ex: Throwable) extends NoBlock
case object ReadOnyMode                       extends NoBlock
case object LockUnavailable                   extends NoBlock
case object NoNewDeploys                      extends NoBlock

object CreateBlockStatus {
  def created(block: BlockMessage.Safe): CreateBlockStatus  = Created(block)
  def internalDeployError(ex: Throwable): CreateBlockStatus = InternalDeployError(ex)
  def readOnlyMode: CreateBlockStatus                       = ReadOnyMode
  def lockUnavailable: CreateBlockStatus                    = LockUnavailable
  def noNewDeploys: CreateBlockStatus                       = NoNewDeploys
}
