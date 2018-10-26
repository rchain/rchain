package coop.rchain.casper.util

import coop.rchain.casper.{BlockDag, MultiParentCasperInstances}
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}
import cats.{Id, Monad}
import cats.implicits._
import cats.mtl.implicits._
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreTestFixture, IndexedBlockDag}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.shared.Time

import scala.collection.immutable.BitSet

class DagOperationsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  val initState = IndexedBlockDag.empty.copy(currentId = -1)

  "bfTraverseF" should "lazily breadth-first traverse a DAG with effectful neighbours" in {
    val stream = DagOperations.bfTraverseF[Id, Int](List(1))(i => List(i * 2, i * 3))
    stream.take(10).toList shouldBe List(1, 2, 3, 4, 6, 9, 8, 12, 18, 27)
  }

  "Greatest common ancestor" should "be computed properly" in {
    /*
     * DAG Looks like this:
     *
     *        b6   b7
     *       |  \ /  \
     *       |   b4  b5
     *       |    \ /
     *       b2    b3
     *         \  /
     *          b1
     *           |
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty)
        b1      <- createBlock[F](Seq(genesis.blockHash))
        b2      <- createBlock[F](Seq(b1.blockHash))
        b3      <- createBlock[F](Seq(b1.blockHash))
        b4      <- createBlock[F](Seq(b3.blockHash))
        b5      <- createBlock[F](Seq(b3.blockHash))
        b6      <- createBlock[F](Seq(b2.blockHash, b4.blockHash))
        b7      <- createBlock[F](Seq(b4.blockHash, b5.blockHash))
      } yield b7

    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val b1 = chain.idToBlocks(1)
    val b2 = chain.idToBlocks(2)
    val b3 = chain.idToBlocks(3)
    val b5 = chain.idToBlocks(5)
    val b6 = chain.idToBlocks(6)
    val b7 = chain.idToBlocks(7)

    DagOperations.greatestCommonAncestorF[Id](b1, b5, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestorF[Id](b3, b2, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestorF[Id](b6, b7, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestorF[Id](b2, b2, genesis, chain) should be(b2)
    DagOperations.greatestCommonAncestorF[Id](b3, b7, genesis, chain) should be(b3)
  }

  "uncommon ancestors" should "be computed properly" in {
    /*
     *  DAG Looks like this:
     *
     *         b6   b7
     *        |  \ / |
     *        b4  b5 |
     *          \ |  |
     *            b3 |
     *            |  |
     *           b1  b2
     *            |  /
     *          genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty)
        b1      <- createBlock[F](Seq(genesis.blockHash))
        b2      <- createBlock[F](Seq(genesis.blockHash))
        b3      <- createBlock[F](Seq(b1.blockHash))
        b4      <- createBlock[F](Seq(b3.blockHash))
        b5      <- createBlock[F](Seq(b3.blockHash))
        b6      <- createBlock[F](Seq(b4.blockHash, b5.blockHash))
        b7      <- createBlock[F](Seq(b2.blockHash, b5.blockHash))
      } yield b7

    val chain      = createChain[StateWithChain].runS(initState)
    val toMetadata = chain.dataLookup

    val b1 = toMetadata(chain.idToBlocks(1).blockHash)
    val b2 = toMetadata(chain.idToBlocks(2).blockHash)
    val b3 = toMetadata(chain.idToBlocks(3).blockHash)
    val b4 = toMetadata(chain.idToBlocks(4).blockHash)
    val b5 = toMetadata(chain.idToBlocks(5).blockHash)
    val b6 = toMetadata(chain.idToBlocks(6).blockHash)
    val b7 = toMetadata(chain.idToBlocks(7).blockHash)

    implicit val ordering = BlockDag.deriveOrdering(chain.dag)
    DagOperations.uncommonAncestors(Vector(b6, b7), chain.dataLookup) shouldBe Map(
      b6 -> BitSet(0),
      b4 -> BitSet(0),
      b7 -> BitSet(1),
      b2 -> BitSet(1)
    )

    DagOperations.uncommonAncestors(Vector(b6, b3), chain.dataLookup) shouldBe Map(
      b6 -> BitSet(0),
      b4 -> BitSet(0),
      b5 -> BitSet(0)
    )

    DagOperations.uncommonAncestors(Vector(b2, b4, b5), chain.dataLookup) shouldBe Map(
      b2 -> BitSet(0),
      b4 -> BitSet(1),
      b5 -> BitSet(2),
      b3 -> BitSet(1, 2),
      b1 -> BitSet(1, 2)
    )

    DagOperations.uncommonAncestors(Vector(b1), chain.dataLookup) shouldBe Map
      .empty[BlockMetadata, BitSet]
  }

}
