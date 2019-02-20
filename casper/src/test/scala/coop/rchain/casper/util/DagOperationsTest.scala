package coop.rchain.casper.util

import org.scalatest.{FlatSpec, Matchers}
import cats.{Id, Monad}
import coop.rchain.casper.BlockHash
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.models.BlockMetadata
import monix.eval.Task

import scala.collection.immutable.BitSet

class DagOperationsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  "bfTraverseF" should "lazily breadth-first traverse a DAG with effectful neighbours" in {
    val stream = DagOperations.bfTraverseF[Id, Int](List(1))(i => List(i * 2, i * 3))
    stream.take(10).toList shouldBe List(1, 2, 3, 4, 6, 9, 8, 12, 18, 27)
  }

  "lowest common ancestor" should "be computed properly" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      def createBlockWithMeta(bh: BlockHash*): Task[BlockMetadata] =
        createBlock[Task](bh.toSeq).map(BlockMetadata.fromBlock)

      implicit def blockMetadataToBlockHash(bm: BlockMetadata): BlockHash = bm.blockHash

      /*
       * DAG Looks like this:
       *
       *        b9   b10
       *          \ /
       *          b8
       *          / \
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
      for {
        genesis <- createBlock[Task](Seq.empty)
        b1      <- createBlockWithMeta(genesis.blockHash)
        b2      <- createBlockWithMeta(b1)
        b3      <- createBlockWithMeta(b1)
        b4      <- createBlockWithMeta(b3)
        b5      <- createBlockWithMeta(b3)
        b6      <- createBlockWithMeta(b2, b4)
        b7      <- createBlockWithMeta(b4, b5)
        b8      <- createBlockWithMeta(b6, b7)
        b9      <- createBlockWithMeta(b8)
        b10     <- createBlockWithMeta(b8)

        dag <- blockDagStorage.getRepresentation

        _      <- DagOperations.lowestCommonAncestorF[Task](b1, b5, dag) shouldBeF b1
        _      <- DagOperations.lowestCommonAncestorF[Task](b3, b2, dag) shouldBeF b1
        _      <- DagOperations.lowestCommonAncestorF[Task](b6, b7, dag) shouldBeF b1
        _      <- DagOperations.lowestCommonAncestorF[Task](b2, b2, dag) shouldBeF b2
        _      <- DagOperations.lowestCommonAncestorF[Task](b10, b9, dag) shouldBeF b8
        result <- DagOperations.lowestCommonAncestorF[Task](b3, b7, dag) shouldBeF b3
      } yield result
  }

  "uncommon ancestors" should "be computed properly" in withStorage {
    implicit blockStore =>
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
        implicit def toMetadata = BlockMetadata.fromBlock _
        for {
          genesis <- createBlock[Task](Seq.empty)
          b1      <- createBlock[Task](Seq(genesis.blockHash))
          b2      <- createBlock[Task](Seq(genesis.blockHash))
          b3      <- createBlock[Task](Seq(b1.blockHash))
          b4      <- createBlock[Task](Seq(b3.blockHash))
          b5      <- createBlock[Task](Seq(b3.blockHash))
          b6      <- createBlock[Task](Seq(b4.blockHash, b5.blockHash))
          b7      <- createBlock[Task](Seq(b2.blockHash, b5.blockHash))

          dag <- blockDagStorage.getRepresentation

          ordering <- dag.deriveOrdering(0L)
          _ <- DagOperations.uncommonAncestors(Vector(b6, b7), dag)(Monad[Task], ordering) shouldBeF Map(
                toMetadata(b6) -> BitSet(0),
                toMetadata(b4) -> BitSet(0),
                toMetadata(b7) -> BitSet(1),
                toMetadata(b2) -> BitSet(1)
              )

          _ <- DagOperations.uncommonAncestors(Vector(b6, b3), dag)(Monad[Task], ordering) shouldBeF Map(
                toMetadata(b6) -> BitSet(0),
                toMetadata(b4) -> BitSet(0),
                toMetadata(b5) -> BitSet(0)
              )

          _ <- DagOperations.uncommonAncestors(Vector(b2, b4, b5), dag)(Monad[Task], ordering) shouldBeF Map(
                toMetadata(b2) -> BitSet(0),
                toMetadata(b4) -> BitSet(1),
                toMetadata(b5) -> BitSet(2),
                toMetadata(b3) -> BitSet(1, 2),
                toMetadata(b1) -> BitSet(1, 2)
              )

          result <- DagOperations.uncommonAncestors(Vector(b1), dag)(Monad[Task], ordering) shouldBeF Map
                     .empty[BlockMetadata, BitSet]
        } yield result
  }

}
