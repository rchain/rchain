package coop.rchain.casper.helper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.hashSignedBlock
import coop.rchain.casper.util.implicits._
import coop.rchain.crypto.PrivateKey
import coop.rchain.models.{BlockHash, Validator}
import coop.rchain.models.BlockHash.BlockHash

import scala.util.Random

object BlockUtil {
  def resignBlock(b: BlockMessage, sk: PrivateKey): BlockMessage = {
    val blockHash =
      hashSignedBlock(b.header, b.body, b.sender, b.sigAlgorithm, b.seqNum, b.shardId, b.extraBytes)
    val sig = ByteString.copyFrom(b.signFunction(blockHash.toByteArray, sk))
    b.copy(blockHash = blockHash, sig = sig)
  }

  def generateValidator(prefix: String = ""): ByteString = {
    val array = Array.ofDim[Byte](Validator.Length)
    Random.nextBytes(array)
    ByteString.copyFrom(array)
  }

  def generateHash(prefix: String = ""): BlockHash = {
    val array = Array.ofDim[Byte](BlockHash.Length)
    Random.nextBytes(array)
    ByteString.copyFrom(array)
  }
}
