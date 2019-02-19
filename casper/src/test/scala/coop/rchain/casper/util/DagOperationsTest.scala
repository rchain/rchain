package coop.rchain.casper.util

import org.scalatest.{FlatSpec, Matchers}
import cats.{Id, Monad}
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

  "Greatest common ancestor" should "be computed properly" in withStorage {
    implicit blockStore =>
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
        for {
          genesis <- createBlock[Task](Seq.empty)
          b1      <- createBlock[Task](Seq(genesis.blockHash))
          b1m     = BlockMetadata.fromBlock(b1)
          b2      <- createBlock[Task](Seq(b1.blockHash))
          b2m     = BlockMetadata.fromBlock(b2)
          b3      <- createBlock[Task](Seq(b1.blockHash))
          b3m     = BlockMetadata.fromBlock(b3)
          b4      <- createBlock[Task](Seq(b3.blockHash))
          b4m     = BlockMetadata.fromBlock(b4)
          b5      <- createBlock[Task](Seq(b3.blockHash))
          b5m     = BlockMetadata.fromBlock(b5)
          b6      <- createBlock[Task](Seq(b2.blockHash, b4.blockHash))
          b6m     = BlockMetadata.fromBlock(b6)
          b7      <- createBlock[Task](Seq(b4.blockHash, b5.blockHash))
          b7m     = BlockMetadata.fromBlock(b7)

          dag      <- blockDagStorage.getRepresentation
          genesism = BlockMetadata.fromBlock(genesis)

          _ <- DagOperations.greatestCommonAncestorF[Task](b1m, b5m, genesism, dag) shouldBeF b1m
          _ <- DagOperations.greatestCommonAncestorF[Task](b3m, b2m, genesism, dag) shouldBeF b1m
          _ <- DagOperations.greatestCommonAncestorF[Task](b6m, b7m, genesism, dag) shouldBeF b1m
          _ <- DagOperations.greatestCommonAncestorF[Task](b2m, b2m, genesism, dag) shouldBeF b2m
          result <- DagOperations
                     .greatestCommonAncestorF[Task](b3m, b7m, genesism, dag) shouldBeF b3m
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
