package coop.rchain.casper

import cats.Applicative
import cats.implicits._
import coop.rchain.casper.protocol.BlockMessage

sealed trait CreateBlockStatus {
  def map(f: BlockMessage => BlockMessage): CreateBlockStatus = this
  def mapF[F[_]: Applicative](f: BlockMessage => F[BlockMessage]): F[CreateBlockStatus] =
    Applicative[F].pure[CreateBlockStatus](this)
}
sealed trait NoBlock extends CreateBlockStatus
final case class Created(block: BlockMessage) extends CreateBlockStatus {
  override def map(f: BlockMessage => BlockMessage): CreateBlockStatus = Created(f(block))
  override def mapF[F[_]: Applicative](f: BlockMessage => F[BlockMessage]): F[CreateBlockStatus] =
    f(block).map(Created.apply)
}
final case class InternalDeployError(ex: Throwable) extends NoBlock
final case object ReadOnlyMode                      extends NoBlock
final case object LockUnavailable                   extends NoBlock
final case object NoNewDeploys                      extends NoBlock
final case object NotEnoughNewBlocks extends NoBlock {
  override def toString: String = "Must wait for more blocks from other validators"
}
final case object TooFarAheadOfLastFinalized extends NoBlock {
  override def toString: String = "Too far ahead of the last finalized block"
}

object CreateBlockStatus {
  def created(block: BlockMessage): CreateBlockStatus       = Created(block)
  def internalDeployError(ex: Throwable): CreateBlockStatus = InternalDeployError(ex)
  def readOnlyMode: CreateBlockStatus                       = ReadOnlyMode
  def lockUnavailable: CreateBlockStatus                    = LockUnavailable
  def noNewDeploys: CreateBlockStatus                       = NoNewDeploys
  def notEnoughNewBlocks: CreateBlockStatus                 = NotEnoughNewBlocks
  def tooFarAheadOfLastFinalized: CreateBlockStatus         = TooFarAheadOfLastFinalized
}
