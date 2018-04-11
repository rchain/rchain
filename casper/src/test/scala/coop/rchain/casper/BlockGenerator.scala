package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{Resource => ResourceProto, _}
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.crypto.hash.Sha256

trait BlockGenerator {
  val resourceCounter: Iterator[Int] = Iterator.from(0)

  def createBlock(parentsHashList: Seq[ByteString]): BlockMessage = {
    val resourceId     = resourceCounter.next()
    val uniqueResource = ResourceProto(ProduceResource(Produce(resourceId)))
    val postState      = RChainState().withResources(Seq(uniqueResource))
    val postStateHash  = Sha256.hash(postState.toByteArray)
    val header = Header()
      .withPostStateHash(ByteString.copyFrom(postStateHash))
      .withParentsHashList(parentsHashList)
    val blockHash = Sha256.hash(header.toByteArray)
    val body      = Body().withPostState(postState)
    BlockMessage(ByteString.copyFrom(blockHash), Some(header), Some(body))
  }
}
