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

  def futureTransaction(b: BlockMessage): ValidBlockProcessing =
    b.state.deploys
      .map(_.deploy)
      .find(_.data.validAfterBlockNumber > b.blockNumber)
      .as(BlockStatus.containsFutureDeploy.asLeft[ValidBlock])
      .getOrElse(BlockStatus.valid.asRight[InvalidBlock])

  def transactionExpiration(b: BlockMessage, expirationThreshold: Int): ValidBlockProcessing = {
    val earliestAcceptableValidAfterBlockNumber = b.blockNumber - expirationThreshold
    b.state.deploys
      .map(_.deploy)
      .find(_.data.validAfterBlockNumber <= earliestAcceptableValidAfterBlockNumber)
      .as(BlockStatus.containsExpiredDeploy.asLeft[ValidBlock])
      .getOrElse(BlockStatus.valid.asRight[InvalidBlock])
  }

  /**
    * Validator should only process deploys from its own shard with shard names in ASCII characters only
    */
  def deploysShardIdentifier(
      b: BlockMessage,
      shardId: String
  ): ValidBlockProcessing = {
    assert(shardId.onlyAscii, "Shard name should contain only ASCII characters")
    if (b.state.deploys.forall(_.deploy.data.shardId == shardId)) {
      BlockStatus.valid.asRight[InvalidBlock]
    } else {
      BlockStatus.invalidDeployShardId.asLeft[ValidBlock]
    }
  }

  /**
    * All of deploys must have greater or equal phloPrice then minPhloPrice
    */
  def phloPrice(
      b: BlockMessage,
      minPhloPrice: Long
  ): ValidBlockProcessing =
    if (b.state.deploys.forall(_.deploy.data.phloPrice >= minPhloPrice)) {
      BlockStatus.valid.asRight[InvalidBlock]
    } else {
      BlockStatus.containsLowCostDeploy.asLeft[ValidBlock]
    }
}