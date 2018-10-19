package coop.rchain.casper.util

import org.scalatest.{FlatSpec, Matchers}
import cats.Id
import coop.rchain.blockstorage.BlockMetadata
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, BlockStoreTestFixture}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.BitSet

class DagOperationsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture
    with BlockDagStorageFixture {

  "bfTraverseF" should "lazily breadth-first traverse a DAG with effectful neighbours" in {
    val stream = DagOperations.bfTraverseF[Id, Int](List(1))(i => List(i * 2, i * 3))
    stream.take(10).toList shouldBe List(1, 2, 3, 4, 6, 9, 8, 12, 18, 27)
  }

  "Greatest common ancestor" should "be computed properly" in withIndexedBlockDagStorage {
    implicit blockDagStorage =>
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
      val genesis = createBlock[Id](Seq.empty)
      val b1      = createBlock[Id](Seq(genesis.blockHash))
      val b2      = createBlock[Id](Seq(b1.blockHash))
      val b3      = createBlock[Id](Seq(b1.blockHash))
      val b4      = createBlock[Id](Seq(b3.blockHash))
      val b5      = createBlock[Id](Seq(b3.blockHash))
      val b6      = createBlock[Id](Seq(b2.blockHash, b4.blockHash))
      val b7      = createBlock[Id](Seq(b4.blockHash, b5.blockHash))

      val dag = blockDagStorage.getRepresentation

      DagOperations.greatestCommonAncestorF[Id](b1, b5, genesis, dag) should be(b1)
      DagOperations.greatestCommonAncestorF[Id](b3, b2, genesis, dag) should be(b1)
      DagOperations.greatestCommonAncestorF[Id](b6, b7, genesis, dag) should be(b1)
      DagOperations.greatestCommonAncestorF[Id](b2, b2, genesis, dag) should be(b2)
      DagOperations.greatestCommonAncestorF[Id](b3, b7, genesis, dag) should be(b3)
  }

  "uncommon ancestors" should "be computed properly" in withIndexedBlockDagStorage {
    implicit blockDagStorage =>
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
      val genesis = createBlock[Id](Seq.empty)
      val b1      = createBlock[Id](Seq(genesis.blockHash))
      val b2      = createBlock[Id](Seq(genesis.blockHash))
      val b3      = createBlock[Id](Seq(b1.blockHash))
      val b4      = createBlock[Id](Seq(b3.blockHash))
      val b5      = createBlock[Id](Seq(b3.blockHash))
      val b6      = createBlock[Id](Seq(b4.blockHash, b5.blockHash))
      val b7      = createBlock[Id](Seq(b2.blockHash, b5.blockHash))

      val dag = blockDagStorage.getRepresentation

      implicit def toMetadata = BlockMetadata.fromBlock _

      implicit val ordering = dag.deriveOrdering(0L)
      DagOperations.uncommonAncestors(Vector(b6, b7), dag) shouldBe Map(
        toMetadata(b6) -> BitSet(0),
        toMetadata(b4) -> BitSet(0),
        toMetadata(b7) -> BitSet(1),
        toMetadata(b2) -> BitSet(1)
      )

      DagOperations.uncommonAncestors(Vector(b6, b3), dag) shouldBe Map(
        toMetadata(b6) -> BitSet(0),
        toMetadata(b4) -> BitSet(0),
        toMetadata(b5) -> BitSet(0)
      )

      DagOperations.uncommonAncestors(Vector(b2, b4, b5), dag) shouldBe Map(
        toMetadata(b2) -> BitSet(0),
        toMetadata(b4) -> BitSet(1),
        toMetadata(b5) -> BitSet(2),
        toMetadata(b3) -> BitSet(1, 2),
        toMetadata(b1) -> BitSet(1, 2)
      )

      DagOperations.uncommonAncestors(Vector(b1), dag) shouldBe Map
        .empty[BlockMetadata, BitSet]
  }

}
