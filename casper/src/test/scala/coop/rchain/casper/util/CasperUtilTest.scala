package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockGenerator
import coop.rchain.casper.internals._
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}

import coop.rchain.catscontrib._, Catscontrib._
import cats._, cats.data._, cats.implicits._, cats.mtl.implicits._

import scala.collection.immutable.HashMap

class CasperUtilTest extends FlatSpec with Matchers with BlockGenerator {

  type StateWithChain[A] = State[Chain, A]

  "isInMainChain" should "classify appropriately" in {
    def createChain[F[_]: Monad: ChainState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq())
        b2      <- createBlock[F](Seq(genesis.blockHash))
        b3      <- createBlock[F](Seq(b2.blockHash))
      } yield b3

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val chain = createChain[StateWithChain].runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    isInMainChain(chain.hashToBlocks, genesis, b3) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b3) should be(true)
    isInMainChain(chain.hashToBlocks, b3, b2) should be(false)
    isInMainChain(chain.hashToBlocks, b3, genesis) should be(false)
  }

  "isInMainChain" should "classify diamond DAGs appropriately" in {
    def createChain[F[_]: Monad: ChainState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq())
        b2      <- createBlock[F](Seq(genesis.blockHash))
        b3      <- createBlock[F](Seq(genesis.blockHash))
        b4      <- createBlock[F](Seq(b2.blockHash, b3.blockHash))
      } yield b4

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val chain = createChain[StateWithChain].runS(initState).value

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

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "isInMainChain" should "classify complicated chains appropriately" in {
    val v1 = ByteString.copyFromUtf8("Validator One")
    val v2 = ByteString.copyFromUtf8("Validator Two")
    def createChain[F[_]: Monad: ChainState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq(), ByteString.EMPTY)
        b2      <- createBlock[F](Seq(genesis.blockHash), v2)
        _       <- createBlock[F](Seq(genesis.blockHash), v1)
        b4      <- createBlock[F](Seq(b2.blockHash), v2)
        _       <- createBlock[F](Seq(b2.blockHash), v1)
        _       <- createBlock[F](Seq(b4.blockHash), v2)
        b7      <- createBlock[F](Seq(b4.blockHash), v1)
        b8      <- createBlock[F](Seq(b7.blockHash), v1)
      } yield b8

    val initState =
      Chain(HashMap.empty[Int, BlockMessage], HashMap.empty[ByteString, BlockMessage], 0)
    val chain = createChain[StateWithChain].runS(initState).value

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)
    val b5      = chain.idToBlocks(5)
    val b6      = chain.idToBlocks(6)
    val b7      = chain.idToBlocks(7)
    val b8      = chain.idToBlocks(8)
    isInMainChain(chain.hashToBlocks, genesis, b2) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b3) should be(false)
    isInMainChain(chain.hashToBlocks, b3, b4) should be(false)
    isInMainChain(chain.hashToBlocks, b4, b5) should be(false)
    isInMainChain(chain.hashToBlocks, b5, b6) should be(false)
    isInMainChain(chain.hashToBlocks, b6, b7) should be(false)
    isInMainChain(chain.hashToBlocks, b7, b8) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b6) should be(true)
    isInMainChain(chain.hashToBlocks, b2, b8) should be(true)
    isInMainChain(chain.hashToBlocks, b4, b2) should be(false)
  }
}
