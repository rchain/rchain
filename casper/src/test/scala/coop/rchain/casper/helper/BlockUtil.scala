package coop.rchain.casper.helper

import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.hashSignedBlock
import coop.rchain.casper.util.implicits._

import scala.util.Random

object BlockUtil {
  def resignBlock(b: BlockMessage, sk: Array[Byte]): BlockMessage = {
    val blockHash =
      hashSignedBlock(b.header.get, b.sender, b.sigAlgorithm, b.seqNum, b.shardId, b.extraBytes)
    val sig = ByteString.copyFrom(b.signFunction(blockHash.toByteArray, sk))
    b.withBlockHash(blockHash).withSig(sig)
  }

  def generateValidator(prefix: String = ""): ByteString =
    ByteString.copyFromUtf8(prefix + Random.nextString(20)).substring(0, 32)

  def generateHash(prefix: String = ""): BlockHash =
    ByteString.copyFromUtf8(prefix + Random.nextString(20)).substring(0, 32)
}
