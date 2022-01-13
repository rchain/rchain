package com.revdefine.node.web.transfer

import coop.rchain.node.web.{
  TransactionInfo,
  CloseBlock => OriCloseBlock,
  PreCharge => OriPreCharge,
  Refund => OriRefund,
  SlashingDeploy => OriSlashingDeploy,
  UserDeploy => OriUserDeploy
}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.syntax._

object Transfer {

  sealed trait TransactionType
  final case class PreCharge()                 extends TransactionType
  final case class Refund()                    extends TransactionType
  final case class UserDeploy()                extends TransactionType
  final case class CloseBlock()                extends TransactionType
  final case class Slashing()                  extends TransactionType
  final case class Genesis()                   extends TransactionType
  final case class CustomType(message: String) extends TransactionType

  val DEFAULT_REASON: String    = "empty"
  val MINT_FROM_ADDRESS: String = "Mint"

  final case class Transaction(
      transactionType: TransactionType,
      fromAddr: String,
      toAddr: String,
      amount: Long,
      blockHash: String,
      blockNumber: Long,
      deployId: String,
      timestamp: Long,
      isFinalized: Boolean,
      isSucceeded: Boolean,
      reason: String
  )

  def fromRnodeTransaction(
      t: TransactionInfo,
      block: BlockMessage,
      isFinalized: Boolean
  ): Transaction = {
    def findDeployIdTimestamp(deployId: String) =
      block.body.deploys
        .find(p => p.deploy.sig.toHexString == deployId)
        .get
        .deploy
        .data
        .timestamp

    val (transactionType: TransactionType, deployId: String, timestamp: Long) =
      t.transactionType match {
        case OriPreCharge(deployId)       => (PreCharge(), deployId, findDeployIdTimestamp(deployId))
        case OriUserDeploy(deployId)      => (UserDeploy(), deployId, findDeployIdTimestamp(deployId))
        case OriRefund(deployId)          => (Refund(), deployId, findDeployIdTimestamp(deployId))
        case OriSlashingDeploy(blockHash) => (Slashing(), blockHash, block.header.timestamp)
        case OriCloseBlock(blockHash)     => (CloseBlock(), blockHash, block.header.timestamp)
      }
    Transaction(
      fromAddr = t.transaction.fromAddr,
      toAddr = t.transaction.toAddr,
      amount = t.transaction.amount,
      transactionType = transactionType,
      blockHash = block.blockHash.toHexString,
      blockNumber = block.body.state.blockNumber,
      deployId = deployId,
      timestamp = timestamp,
      isFinalized = isFinalized,
      isSucceeded = t.transaction.failReason.isEmpty,
      reason = t.transaction.failReason.getOrElse(DEFAULT_REASON)
    )
  }
}
