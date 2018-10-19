package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Bond
import org.scalatest.{FlatSpec, Matchers}
import cats._
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, BlockStoreFixture}
import coop.rchain.casper.helper.BlockGenerator._

import scala.collection.immutable.{HashMap, HashSet}

class CliqueOracleTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreFixture
    with BlockDagStorageFixture {

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Turan Oracle" should "detect finality as appropriate" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val v1     = ByteString.copyFromUtf8("Validator One")
      val v2     = ByteString.copyFromUtf8("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)

      val genesis = createBlock[Id](Seq(), ByteString.EMPTY, bonds)
      val b2 = createBlock[Id](
        Seq(genesis.blockHash),
        v2,
        bonds,
        HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
      )
      val b3 = createBlock[Id](
        Seq(genesis.blockHash),
        v1,
        bonds,
        HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
      )
      val b4 = createBlock[Id](
        Seq(b2.blockHash),
        v2,
        bonds,
        HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
      )
      val b5 = createBlock[Id](
        Seq(b2.blockHash),
        v1,
        bonds,
        HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
      )
      val b6 = createBlock[Id](
        Seq(b4.blockHash),
        v2,
        bonds,
        HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
      )
      val b7 = createBlock[Id](
        Seq(b4.blockHash),
        v1,
        bonds,
        HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
      )
      val b8 = createBlock[Id](
        Seq(b7.blockHash),
        v1,
        bonds,
        HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
      )

      val dag = blockDagStorage.getRepresentation

      implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]

      val genesisFaultTolerance =
        SafetyOracle[Id].normalizedFaultTolerance(dag, genesis.blockHash)
      assert(genesisFaultTolerance == 1)
      val b2FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b2.blockHash)
      assert(b2FaultTolerance == 1)
      val b3FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b3.blockHash)
      assert(b3FaultTolerance == -1)
      val b4FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b4.blockHash)
      assert(b4FaultTolerance == -0.2f) // Clique oracle would be 0.2f
    }
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Turan Oracle" should "detect possible disagreements appropriately" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val v1      = ByteString.copyFromUtf8("Validator One")
        val v2      = ByteString.copyFromUtf8("Validator Two")
        val v3      = ByteString.copyFromUtf8("Validator Three")
        val v1Bond  = Bond(v1, 25)
        val v2Bond  = Bond(v2, 20)
        val v3Bond  = Bond(v3, 15)
        val bonds   = Seq(v1Bond, v2Bond, v3Bond)
        val genesis = createBlock[Id](Seq(), ByteString.EMPTY, bonds)
        val b2 = createBlock[Id](
          Seq(genesis.blockHash),
          v2,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
        )
        val b3 = createBlock[Id](
          Seq(genesis.blockHash),
          v1,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
        )
        val b4 = createBlock[Id](
          Seq(b2.blockHash),
          v3,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
        )
        val b5 = createBlock[Id](
          Seq(b3.blockHash),
          v2,
          bonds,
          HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
        )
        val b6 = createBlock[Id](
          Seq(b4.blockHash),
          v1,
          bonds,
          HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
        )
        val b7 = createBlock[Id](
          Seq(b5.blockHash),
          v3,
          bonds,
          HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
        )
        val b8 = createBlock[Id](
          Seq(b6.blockHash),
          v2,
          bonds,
          HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
        )

        val dag = blockDagStorage.getRepresentation

        implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]

        val genesisFaultTolerance =
          SafetyOracle[Id].normalizedFaultTolerance(dag, genesis.blockHash)
        assert(genesisFaultTolerance == 1)
        val b2FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b2.blockHash)
        assert(b2FaultTolerance == -0.5f)
        val b3FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b3.blockHash)
        assert(b3FaultTolerance == -1f)
        val b4FaultTolerance = SafetyOracle[Id].normalizedFaultTolerance(dag, b4.blockHash)
        assert(b4FaultTolerance == -0.5f)
      }
  }
}
