package coop.rchain.casper.util

import ProtoUtil._
import com.google.protobuf.ByteString
import coop.rchain.casper.{BlockDag, MultiParentCasperInstances}
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.effect.Bracket
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.shared.Time

import scala.collection.immutable.{HashMap, HashSet}

class CasperUtilTest extends FlatSpec with Matchers with BlockGenerator with BlockStoreFixture {
  val initState = BlockDag()

  "isInMainChain" should "classify appropriately" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq())
        b2      <- createBlock[F](Seq(genesis.blockHash))
        b3      <- createBlock[F](Seq(b2.blockHash))
      } yield b3
    val chain = createChain[StateWithChain].runS(initState)

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    isInMainChain(BlockStore[Id].asMap(), genesis, b3) should be(true)
    isInMainChain(BlockStore[Id].asMap(), b2, b3) should be(true)
    isInMainChain(BlockStore[Id].asMap(), b3, b2) should be(false)
    isInMainChain(BlockStore[Id].asMap(), b3, genesis) should be(false)
  }

  "isInMainChain" should "classify diamond DAGs appropriately" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq())
        b2      <- createBlock[F](Seq(genesis.blockHash))
        b3      <- createBlock[F](Seq(genesis.blockHash))
        b4      <- createBlock[F](Seq(b2.blockHash, b3.blockHash))
      } yield b4

    val chain = createChain[StateWithChain].runS(initState)

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)
    isInMainChain(BlockStore[Id].asMap(), genesis, b2) should be(true)
    isInMainChain(BlockStore[Id].asMap(), genesis, b3) should be(true)
    isInMainChain(BlockStore[Id].asMap(), genesis, b4) should be(true)
    isInMainChain(BlockStore[Id].asMap(), b2, b4) should be(true)
    isInMainChain(BlockStore[Id].asMap(), b3, b4) should be(false)
  }

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "isInMainChain" should "classify complicated chains appropriately" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val v1                       = ByteString.copyFromUtf8("Validator One")
      val v2                       = ByteString.copyFromUtf8("Validator Two")
      def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
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

      val chain = createChain[StateWithChain].runS(initState)

      val genesis = chain.idToBlocks(1)
      val b2      = chain.idToBlocks(2)
      val b3      = chain.idToBlocks(3)
      val b4      = chain.idToBlocks(4)
      val b5      = chain.idToBlocks(5)
      val b6      = chain.idToBlocks(6)
      val b7      = chain.idToBlocks(7)
      val b8      = chain.idToBlocks(8)
      isInMainChain(BlockStore[Id].asMap(), genesis, b2) should be(true)
      isInMainChain(BlockStore[Id].asMap(), b2, b3) should be(false)
      isInMainChain(BlockStore[Id].asMap(), b3, b4) should be(false)
      isInMainChain(BlockStore[Id].asMap(), b4, b5) should be(false)
      isInMainChain(BlockStore[Id].asMap(), b5, b6) should be(false)
      isInMainChain(BlockStore[Id].asMap(), b6, b7) should be(false)
      isInMainChain(BlockStore[Id].asMap(), b7, b8) should be(true)
      isInMainChain(BlockStore[Id].asMap(), b2, b6) should be(true)
      isInMainChain(BlockStore[Id].asMap(), b2, b8) should be(true)
      isInMainChain(BlockStore[Id].asMap(), b4, b2) should be(false)
  }

  /*
   * DAG Looks like this:
   *
   *       b9      b10
   *        \      /
   *        b7   b8
   *          \  /
   *           b6
   *           / \
   *      b4  /   \  b5
   *       | /     \ |
   *       b2       b3
   *        \       /
   *         genesis
   */
  "Blocks" should "conflict if they use the same deploys in different histories" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val deploys                  = (0 until 6).map(basicDeployCost)

      def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
        for {
          genesis <- createBlock[F](Seq())
          b2      <- createBlock[F](Seq(genesis.blockHash), deploys = Seq(deploys(0)))
          b3      <- createBlock[F](Seq(genesis.blockHash), deploys = Seq(deploys(1)))
          b4      <- createBlock[F](Seq(b2.blockHash), deploys = Seq(deploys(2)))
          b5      <- createBlock[F](Seq(b3.blockHash), deploys = Seq(deploys(2)))
          b6      <- createBlock[F](Seq(b2.blockHash, b3.blockHash), deploys = Seq(deploys(2)))
          b7      <- createBlock[F](Seq(b6.blockHash), deploys = Seq(deploys(3)))
          b8      <- createBlock[F](Seq(b6.blockHash), deploys = Seq(deploys(5)))
          b9      <- createBlock[F](Seq(b7.blockHash), deploys = Seq(deploys(5)))
          b10     <- createBlock[F](Seq(b8.blockHash), deploys = Seq(deploys(4)))
        } yield b10

      val chain   = createChain[StateWithChain].runS(initState)
      val genesis = chain.idToBlocks(1)

      val b2  = chain.idToBlocks(2)
      val b3  = chain.idToBlocks(3)
      val b4  = chain.idToBlocks(4)
      val b5  = chain.idToBlocks(5)
      val b6  = chain.idToBlocks(6)
      val b7  = chain.idToBlocks(7)
      val b8  = chain.idToBlocks(8)
      val b9  = chain.idToBlocks(9)
      val b10 = chain.idToBlocks(10)

      conflicts(b2, b3, genesis, chain, BlockStore[Id].asMap()) should be(false)
      conflicts(b4, b5, genesis, chain, BlockStore[Id].asMap()) should be(true)
      conflicts(b6, b6, genesis, chain, BlockStore[Id].asMap()) should be(false)
      conflicts(b6, b9, genesis, chain, BlockStore[Id].asMap()) should be(false)
      conflicts(b7, b8, genesis, chain, BlockStore[Id].asMap()) should be(false)
      conflicts(b7, b10, genesis, chain, BlockStore[Id].asMap()) should be(false)
      conflicts(b9, b10, genesis, chain, BlockStore[Id].asMap()) should be(true)
  }
}
