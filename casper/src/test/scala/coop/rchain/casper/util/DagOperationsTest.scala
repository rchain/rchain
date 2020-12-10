package coop.rchain.casper.util

import cats.{Id, Monad}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.models.BlockMetadata
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.DagOps
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class DagOperationsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  "bfTraverseF" should "lazily breadth-first traverse a DAG with effectful neighbours" in {
    val stream = DagOps.bfTraverseF[Id, Int](List(1))(i => List(i * 2, i * 3))
    stream.take(10).toList shouldBe List(1, 2, 3, 4, 6, 9, 8, 12, 18, 27)
  }

  "lowest common universal ancestor" should "be computed properly" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      def createBlockWithMeta(genesis: BlockMessage, bh: BlockHash*): Task[BlockMetadata] =
        createBlock[Task](bh.toSeq, genesis).map(b => BlockMetadata.fromBlock(b, false))

      def createBlockWithMetaAndSeq(
          genesis: BlockMessage,
          seqNum: Int,
          bh: BlockHash*
      ): Task[BlockMetadata] =
        createBlock[Task](bh.toSeq, genesis, seqNum = seqNum)
          .map(b => BlockMetadata.fromBlock(b, false))

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
        genesis <- createGenesis[Task]()
        b1      <- createBlockWithMeta(genesis, genesis.blockHash)
        b2      <- createBlockWithMetaAndSeq(genesis, seqNum = 2, b1)
        b3      <- createBlockWithMetaAndSeq(genesis, seqNum = 2, b1)
        b4      <- createBlockWithMeta(genesis, b3)
        b5      <- createBlockWithMeta(genesis, b3)
        b6      <- createBlockWithMeta(genesis, b2, b4)
        b7      <- createBlockWithMeta(genesis, b4, b5)
        b8      <- createBlockWithMeta(genesis, b6, b7)
        b9      <- createBlockWithMeta(genesis, b8)
        b10     <- createBlockWithMeta(genesis, b8)

        dag <- blockDagStorage.getRepresentation

        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b1, b5, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b2, b3, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b3, b2, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b6, b7, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b2, b2, dag) shouldBeF b2
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b10, b9, dag) shouldBeF b8
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b3, b7, dag) shouldBeF b3
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b3, b8, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b4, b5, dag) shouldBeF b3
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b4, b6, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b7, b7, dag) shouldBeF b7
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b7, b8, dag) shouldBeF b1
        _ <- DagOperations.lowestUniversalCommonAncestorF[Task](b8, b9, dag) shouldBeF b8
      } yield ()
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
        implicit def toMetadata(b: BlockMessage) = BlockMetadata.fromBlock(b, false)
        for {
          genesis <- createGenesis[Task]()
          b1      <- createBlock[Task](Seq(genesis.blockHash), genesis)
          b2      <- createBlock[Task](Seq(genesis.blockHash), genesis)
          b3      <- createBlock[Task](Seq(b1.blockHash), genesis)
          b4      <- createBlock[Task](Seq(b3.blockHash), genesis)
          b5      <- createBlock[Task](Seq(b3.blockHash), genesis)
          b6      <- createBlock[Task](Seq(b4.blockHash, b5.blockHash), genesis)
          b7      <- createBlock[Task](Seq(b2.blockHash, b5.blockHash), genesis)

          dag <- blockDagStorage.getRepresentation

          _ <- DagOperations.uncommonAncestors(Vector(b6, b7), dag)(Monad[Task]) shouldBeF Map(
                toMetadata(b6) -> BitSet(0),
                toMetadata(b4) -> BitSet(0),
                toMetadata(b7) -> BitSet(1),
                toMetadata(b2) -> BitSet(1)
              )

          _ <- DagOperations.uncommonAncestors(Vector(b6, b3), dag)(Monad[Task]) shouldBeF Map(
                toMetadata(b6) -> BitSet(0),
                toMetadata(b4) -> BitSet(0),
                toMetadata(b5) -> BitSet(0)
              )

          _ <- DagOperations.uncommonAncestors(Vector(b2, b4, b5), dag)(Monad[Task]) shouldBeF Map(
                toMetadata(b2) -> BitSet(0),
                toMetadata(b4) -> BitSet(1),
                toMetadata(b5) -> BitSet(2),
                toMetadata(b3) -> BitSet(1, 2),
                toMetadata(b1) -> BitSet(1, 2)
              )

          result <- DagOperations.uncommonAncestors(Vector(b1), dag)(Monad[Task]) shouldBeF Map
                     .empty[BlockMetadata, BitSet]
        } yield result
  }

}
