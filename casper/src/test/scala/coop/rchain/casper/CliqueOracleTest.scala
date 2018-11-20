package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Bond
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.helper.BlockGenerator._
import monix.eval.Task

import scala.collection.immutable.{HashMap, HashSet}

class CliqueOracleTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "Turan Oracle" should "detect finality as appropriate" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = ByteString.copyFromUtf8("Validator One")
      val v2     = ByteString.copyFromUtf8("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)

      implicit val turanOracleEffect = SafetyOracle.turanOracle[Task]

      for {
        genesis <- createBlock[Task](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b2.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               v2,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b4.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b7.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
             )
        dag                   <- blockDagStorage.getRepresentation
        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance == 1)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance == 1)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance == -1)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance == -0.2f) // Clique oracle would be 0.2f
      } yield result
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  "Turan Oracle" should "detect possible disagreements appropriately" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = ByteString.copyFromUtf8("Validator One")
      val v2     = ByteString.copyFromUtf8("Validator Two")
      val v3     = ByteString.copyFromUtf8("Validator Three")
      val v1Bond = Bond(v1, 25)
      val v2Bond = Bond(v2, 20)
      val v3Bond = Bond(v3, 15)
      val bonds  = Seq(v1Bond, v2Bond, v3Bond)

      implicit val turanOracleEffect = SafetyOracle.turanOracle[Task]
      for {
        genesis <- createBlock[Task](Seq(), ByteString.EMPTY, bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               v3,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b3.blockHash),
               v2,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b5.blockHash),
               v3,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b6.blockHash),
               v2,
               bonds,
               HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )

        dag <- blockDagStorage.getRepresentation

        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance == 1)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance == -0.5f)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance == -1f)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance == -0.5f)
      } yield result
  }
}
