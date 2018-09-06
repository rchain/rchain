package coop.rchain.casper.helper
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Body, Header, RChainState}
import coop.rchain.casper.util.ProtoUtil.hashSignedBlock
import coop.rchain.casper.util.implicits._

object BlockUtil {
  val emptySafeBlock = BlockMessage.BlockMessageSafe
    .create(
      BlockMessage().withHeader(Header()).withBody(Body().withPostState(RChainState()))
    )
    .get

  def resignBlock(b: BlockMessage.BlockMessageSafe,
                  sk: Array[Byte]): BlockMessage.BlockMessageSafe = {
    val blockHash =
      hashSignedBlock(b.header, b.sender, b.sigAlgorithm, b.seqNum, b.shardId, b.extraBytes)
    val sig = ByteString.copyFrom(b.signFunction(blockHash.toByteArray, sk))
    b.withBlockHash(blockHash).withSig(sig)
  }
}
