package coop.rchain.casper

import cats.syntax.all._
import coop.rchain.casper.Validate.signatureVerifiers
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.models.BlockVersion
import coop.rchain.models.syntax._

import scala.util.{Success, Try}

object BlockValidationLogic {
  def version(b: BlockMessage): Boolean = BlockVersion.Supported.contains(b.version)

  def blockHash(b: BlockMessage): Boolean = b.blockHash == ProtoUtil.hashBlock(b)

  def formatOfFields(b: BlockMessage): Boolean =
    b.blockHash.nonEmpty &&
      b.sig.nonEmpty &&
      b.sigAlgorithm.nonEmpty &&
      b.shardId.nonEmpty &&
      b.postStateHash.nonEmpty

  def blockSignature(b: BlockMessage): Boolean =
    signatureVerifiers
      .get(b.sigAlgorithm)
      .exists(verify => {
        Try(verify(b.blockHash.toByteArray, b.sig.toByteArray, b.sender.toByteArray)) match {
          case Success(true) => true
          case _             => false
        }
      })

  def futureTransaction(b: BlockMessage): Boolean =
    !b.state.deploys.map(_.deploy).exists(_.data.validAfterBlockNumber > b.blockNumber)

  def transactionExpiration(b: BlockMessage, expirationThreshold: Int): Boolean = {
    val earliestAcceptableValidAfterBlockNumber = b.blockNumber - expirationThreshold
    !b.state.deploys
      .map(_.deploy)
      .exists(_.data.validAfterBlockNumber <= earliestAcceptableValidAfterBlockNumber)
  }

  /**
    * Validator should only process deploys from its own shard with shard names in ASCII characters only
    */
  def deploysShardIdentifier(b: BlockMessage, shardId: String): Boolean = {
    assert(shardId.onlyAscii, "Shard name should contain only ASCII characters")
    b.state.deploys.forall(_.deploy.data.shardId == shardId)
  }

  /**
    * All of deploys must have greater or equal phloPrice then minPhloPrice
    */
  def phloPrice(b: BlockMessage, minPhloPrice: Long): Boolean =
    b.state.deploys.forall(_.deploy.data.phloPrice >= minPhloPrice)
}
