package coop.rchain.casper

import cats.Monad
import cats.implicits._
import coop.rchain.casper.protocol.BlockMessage

sealed trait CreateBlockStatus {
  def map(f: BlockMessage => BlockMessage): CreateBlockStatus = this
  def flatMap[F[_]: Monad](f: BlockMessage => F[BlockMessage]): F[CreateBlockStatus] =
    Monad[F].pure[CreateBlockStatus](this)
}
sealed trait NoBlock extends CreateBlockStatus
case class Created(block: BlockMessage) extends CreateBlockStatus {
  override def map(f: BlockMessage => BlockMessage): CreateBlockStatus = Created(f(block))
  override def flatMap[F[_]: Monad](f: BlockMessage => F[BlockMessage]): F[CreateBlockStatus] =
    f(block).map(Created.apply)
}
case class InternalDeployError(ex: Throwable) extends NoBlock
case object ReadOnyMode                       extends NoBlock
case object LockUnavailable                   extends NoBlock
case object NoNewDeploys                      extends NoBlock

object CreateBlockStatus {
  def created(block: BlockMessage): CreateBlockStatus       = Created(block)
  def internalDeployError(ex: Throwable): CreateBlockStatus = InternalDeployError(ex)
  def readOnlyMode: CreateBlockStatus                       = ReadOnyMode
  def lockUnavailable: CreateBlockStatus                    = LockUnavailable
  def noNewDeploys: CreateBlockStatus                       = NoNewDeploys
}
