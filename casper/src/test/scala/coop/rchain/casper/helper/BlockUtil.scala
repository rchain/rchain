package coop.rchain.casper.helper
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.hashSignedBlock
import coop.rchain.casper.util.implicits._

object BlockUtil {
  def resignBlock(b: BlockMessage, sk: Array[Byte]): BlockMessage = {
    val blockHash =
      hashSignedBlock(b.header.get, b.sender, b.sigAlgorithm, b.seqNum, b.shardId, b.extraBytes)
    val sig = ByteString.copyFrom(b.signFunction(blockHash.toByteArray, sk))
    b.withBlockHash(blockHash).withSig(sig)
  }
}
