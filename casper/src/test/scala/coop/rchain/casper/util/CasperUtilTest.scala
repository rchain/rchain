package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.crypto.hash.Sha256

import scala.collection.mutable

class CasperUtilTest extends FlatSpec with Matchers {
  val resourceCounter: Iterator[Int] = Iterator.from(0)

  "isInMainChain" should "classify appropriately" in {
    val chain = mutable.HashMap[ByteString, BlockMessage]()

    val genesis: BlockMessage = createBlock(Seq())
    chain += (genesis.blockHash -> genesis)

    val blockMessage2: BlockMessage = createBlock(Seq(genesis.blockHash))
    chain += (blockMessage2.blockHash -> blockMessage2)

    val blockMessage3: BlockMessage = createBlock(Seq(blockMessage2.blockHash))
    chain += (blockMessage3.blockHash -> blockMessage3)

    isInMainChain(chain, genesis, blockMessage3) should be(true)
    isInMainChain(chain, blockMessage2, blockMessage3) should be(true)
    isInMainChain(chain, blockMessage3, blockMessage2) should be(false)
    isInMainChain(chain, blockMessage3, genesis) should be(false)
  }

  "isInMainChain" should "classify diamond DAGs appropriately" in {
    val chain = mutable.HashMap[ByteString, BlockMessage]()

    val genesis: BlockMessage = createBlock(Seq())
    chain += (genesis.blockHash -> genesis)

    val blockMessage2: BlockMessage = createBlock(Seq(genesis.blockHash))
    chain += (blockMessage2.blockHash -> blockMessage2)

    val blockMessage3: BlockMessage = createBlock(Seq(genesis.blockHash))
    chain += (blockMessage3.blockHash -> blockMessage3)

    val blockMessage4: BlockMessage =
      createBlock(Seq(blockMessage2.blockHash, blockMessage3.blockHash))
    chain += (blockMessage4.blockHash -> blockMessage4)

    isInMainChain(chain, genesis, blockMessage2) should be(true)
    isInMainChain(chain, genesis, blockMessage3) should be(true)
    isInMainChain(chain, genesis, blockMessage4) should be(true)
    isInMainChain(chain, blockMessage2, blockMessage4) should be(true)
    isInMainChain(chain, blockMessage3, blockMessage4) should be(false)
  }

  private def createBlock(parentsHashList: Seq[ByteString]) = {
    val resourceId     = resourceCounter.next()
    val uniqueResource = Resource(ProduceResource(Produce(resourceId)))
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
