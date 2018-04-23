package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockGenerator
import coop.rchain.casper.internals._
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}
import scalaz.{State}

import scala.collection.immutable.HashMap

class CasperUtilTest extends FlatSpec with Matchers with BlockGenerator {

  type StateWithChain[A] = State[Chain, A]

  "isInMainChain" should "classify appropriately" in {
    val createChain =
      for {
        genesis <- createBlock[StateWithChain](Seq())
        b2      <- createBlock[StateWithChain](Seq(genesis.blockHash))
        b3      <- createBlock[StateWithChain](Seq(b2.blockHash))
      } yield b3

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val (chain: Chain, lastBlock: BlockMessage) = createChain.run(initState)

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    isInMainChain(chain.hashToBlocks, genesis, b3) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b3) should be(true)
    isInMainChain(chain.hashToBlocks, b3, b2) should be(false)
    isInMainChain(chain.hashToBlocks, b3, genesis) should be(false)
  }

  "isInMainChain" should "classify diamond DAGs appropriately" in {
    val createChain =
      for {
        genesis <- createBlock[StateWithChain](Seq())
        b2      <- createBlock[StateWithChain](Seq(genesis.blockHash))
        b3      <- createBlock[StateWithChain](Seq(genesis.blockHash))
        b4      <- createBlock[StateWithChain](Seq(b2.blockHash, b3.blockHash))
      } yield b4

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val (chain: Chain, lastBlock: BlockMessage) = createChain.run(initState)

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)
    isInMainChain(chain.hashToBlocks, genesis, b2) should be(true)
    isInMainChain(chain.hashToBlocks, genesis, b3) should be(true)
    isInMainChain(chain.hashToBlocks, genesis, b4) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b4) should be(true)
    isInMainChain(chain.hashToBlocks, b3, b4) should be(false)
  }
}
