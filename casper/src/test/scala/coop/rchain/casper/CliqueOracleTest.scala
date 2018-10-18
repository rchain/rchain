package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib._
import Catscontrib._
import cats._
import cats.data._
import cats.effect.Bracket
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture, IndexedBlockDag}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.{HashMap, HashSet}

class CliqueOracleTest extends FlatSpec with Matchers with BlockGenerator with BlockStoreFixture {
  val initState = IndexedBlockDag.empty.withOffset(1L)

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Turan Oracle" should "detect finality as appropriate" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val v1                       = ByteString.copyFromUtf8("Validator One")
    val v2                       = ByteString.copyFromUtf8("Validator Two")
    val v1Bond                   = Bond(v1, 2)
    val v2Bond                   = Bond(v2, 3)
    val bonds                    = Seq(v1Bond, v2Bond)
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[F](
               Seq(genesis.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b3 <- createBlock[F](
               Seq(genesis.blockHash),
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b4 <- createBlock[F](
               Seq(b2.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
             )
        b5 <- createBlock[F](
               Seq(b2.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
             )
        _ <- createBlock[F](
              Seq(b4.blockHash),
              v2,
              bonds,
              HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
            )
        b7 <- createBlock[F](
               Seq(b4.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b8 <- createBlock[F](
               Seq(b7.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
             )
      } yield b8

    val chain: IndexedBlockDag = createChain[StateWithChain].runS(initState)

    val genesis = chain.idToBlocks(1)
    val b2      = chain.idToBlocks(2)
    val b3      = chain.idToBlocks(3)
    val b4      = chain.idToBlocks(4)

    implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]

    def runSafetyOracle[F[_]: Monad: SafetyOracle]: Unit = {
      val genesisFaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, genesis.blockHash)
      assert(genesisFaultTolerance == 1)
      val b2FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b2.blockHash)
      assert(b2FaultTolerance == 1)
      val b3FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b3.blockHash)
      assert(b3FaultTolerance == -1)
      val b4FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b4.blockHash)
      assert(b4FaultTolerance == -0.2f) // Clique oracle would be 0.2f
    }
    runSafetyOracle[Id]
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Turan Oracle" should "detect possible disagreements appropriately" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val v1                       = ByteString.copyFromUtf8("Validator One")
      val v2                       = ByteString.copyFromUtf8("Validator Two")
      val v3                       = ByteString.copyFromUtf8("Validator Three")
      val v1Bond                   = Bond(v1, 25)
      val v2Bond                   = Bond(v2, 20)
      val v3Bond                   = Bond(v3, 15)
      val bonds                    = Seq(v1Bond, v2Bond, v3Bond)
      def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
        for {
          genesis <- createBlock[F](Seq(), ByteString.EMPTY, bonds)
          b2 <- createBlock[F](
                 Seq(genesis.blockHash),
                 v2,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b3 <- createBlock[F](
                 Seq(genesis.blockHash),
                 v1,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b4 <- createBlock[F](
                 Seq(b2.blockHash),
                 v3,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
               )
          b5 <- createBlock[F](
                 Seq(b3.blockHash),
                 v2,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
               )
          b6 <- createBlock[F](
                 Seq(b4.blockHash),
                 v1,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
               )
          b7 <- createBlock[F](
                 Seq(b5.blockHash),
                 v3,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
          b8 <- createBlock[F](
                 Seq(b6.blockHash),
                 v2,
                 bonds,
                 HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
        } yield b8

      val chain: IndexedBlockDag = createChain[StateWithChain].runS(initState)

      val genesisBlockHash = chain.idToBlocks(1).blockHash
      val b2BlockHash      = chain.idToBlocks(2).blockHash
      val b3BlockHash      = chain.idToBlocks(3).blockHash
      val b4BlockHash      = chain.idToBlocks(4).blockHash

      implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]

      def runSafetyOracle[F[_]: Monad: SafetyOracle]: Unit = {
        val genesisFaultTolerance =
          SafetyOracle[F].normalizedFaultTolerance(chain, genesisBlockHash)
        assert(genesisFaultTolerance == 1)
        val b2FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b2BlockHash)
        assert(b2FaultTolerance == -0.5f)
        val b3FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b3BlockHash)
        assert(b3FaultTolerance == -1f)
        val b4FaultTolerance = SafetyOracle[F].normalizedFaultTolerance(chain, b4BlockHash)
        assert(b4FaultTolerance == -0.5f)
      }
      runSafetyOracle[Id]
  }
}
