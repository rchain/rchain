package coop.rchain.casper.util

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import org.scalatest.{FlatSpec, Matchers}
import cats.Monad
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import coop.rchain.casper.helper.BlockGenerator
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.shared.Time

class DagOperationsTest extends FlatSpec with Matchers with BlockGenerator {
  val initState = BlockDag().copy(currentId = -1)

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
    def createChain[F[_]: Monad: BlockDagState: Time]: F[BlockMessage] =
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

    DagOperations.greatestCommonAncestor(b1, b5, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestor(b3, b2, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestor(b6, b7, genesis, chain) should be(b1)
    DagOperations.greatestCommonAncestor(b2, b2, genesis, chain) should be(b2)
    DagOperations.greatestCommonAncestor(b3, b7, genesis, chain) should be(b3)
  }

}
