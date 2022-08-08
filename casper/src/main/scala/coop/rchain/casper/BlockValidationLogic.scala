package coop.rchain.casper

import coop.rchain.casper.Validate.signatureVerifiers
import coop.rchain.casper.protocol.BlockMessage

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
}
