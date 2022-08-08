package coop.rchain.casper

import cats.syntax.all._
import coop.rchain.casper.Validate.signatureVerifiers
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockVersion
import coop.rchain.models.syntax._

import scala.util.{Success, Try}

object BlockValidationLogic {
  def blockSignature(b: BlockMessage): Boolean =
    signatureVerifiers
      .get(b.sigAlgorithm)
      .exists(verify => {
        Try(verify(b.blockHash.toByteArray, b.sig.toByteArray, b.sender.toByteArray)) match {
          case Success(true) => true
          case _             => false
        }
      })

  def formatOfFields(b: BlockMessage): Boolean =
    b.blockHash.nonEmpty &&
      b.sig.nonEmpty &&
      b.sigAlgorithm.nonEmpty &&
      b.shardId.nonEmpty &&
      b.postStateHash.nonEmpty
  def version(b: BlockMessage): Boolean = BlockVersion.Supported.contains(b.version)

  def futureTransaction(b: BlockMessage): ValidBlockProcessing =
    b.state.deploys
      .map(_.deploy)
      .find(_.data.validAfterBlockNumber > b.blockNumber)
      .as(BlockStatus.containsFutureDeploy.asLeft[ValidBlock])
      .getOrElse(BlockStatus.valid.asRight[InvalidBlock])
}
