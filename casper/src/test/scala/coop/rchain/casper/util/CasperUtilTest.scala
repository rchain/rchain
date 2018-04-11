package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockGenerator
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class CasperUtilTest extends FlatSpec with Matchers with BlockGenerator {

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
}
