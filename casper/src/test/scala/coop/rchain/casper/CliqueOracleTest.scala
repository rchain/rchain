package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.{BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import org.scalatest._
import org.scalatest.Matchers._

import scala.collection.immutable.HashMap

class CliqueOracleTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  behavior of "Clique Oracle"

  implicit val logEff = new LogStub[Task]

  def createBlock(bonds: Seq[Bond])(creator: ByteString)(
      parent: BlockMessage,
      justifications: Map[Validator, BlockMessage]
  )(implicit store: BlockStore[Task], dagStore: IndexedBlockDagStorage[Task]): Task[BlockMessage] =
    createBlock[Task](
      Seq(parent.blockHash),
      creator,
      bonds,
      justifications.map { case (v, bm) => (v, bm.blockHash) }
    )

  // See [[/docs/casper/images/cbc-casper_ping_pong_diagram.png]]
  it should "detect finality as appropriate" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1             = generateValidator("Validator One")
      val v2             = generateValidator("Validator Two")
      val v1Bond         = Bond(v1, 2)
      val v2Bond         = Bond(v2, 3)
      val bonds          = Seq(v1Bond, v2Bond)
      val blockCreatorV1 = createBlock(bonds)(v1) _
      val blockCreatorV2 = createBlock(bonds)(v2) _

      implicit val cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
      for {
        genesis <- createBlock[Task](Seq(), ByteString.EMPTY, bonds)
        genesisJustification = HashMap(v1 -> genesis, v2 -> genesis)
        b2 <- blockCreatorV2(genesis, genesisJustification)
        b3 <- blockCreatorV1(genesis, genesisJustification)
        b4 <- blockCreatorV2(b2, HashMap(v1 -> genesis, v2 -> b2))
        b5 <- blockCreatorV1(b2, HashMap(v1 -> b3, v2 -> b2))
        b6 <- blockCreatorV2(b4, HashMap(v1 -> b5, v2 -> b4))
        b7 <- blockCreatorV1(b4, HashMap(v1 -> b5, v2 -> b4))
        b8 <- blockCreatorV1(b7, HashMap(v1 -> b7, v2 -> b4))

        dag                   <- blockDagStorage.getRepresentation
        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance === 1f +- 0.01f)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance === 1f +- 0.01f)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance === -1f +- 0.01f)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance === 0.2f +- 0.01f)
      } yield result
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  it should "detect possible disagreements appropriately" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v3     = generateValidator("Validator Three")
      val v1Bond = Bond(v1, 25)
      val v2Bond = Bond(v2, 20)
      val v3Bond = Bond(v3, 15)
      val bonds  = Seq(v1Bond, v2Bond, v3Bond)
      val blockCreatorV1 = createBlock(bonds)(v1) _
      val blockCreatorV2 = createBlock(bonds)(v2) _
      val blockCreatorV3 = createBlock(bonds)(v3) _

      implicit val cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
      for {
        genesis <- createBlock[Task](Seq(), ByteString.EMPTY, bonds)
        genesisJustification = HashMap(v1 -> genesis, v2 -> genesis, v3 -> genesis)
        b2 <- blockCreatorV2(genesis, genesisJustification)
        b3 <- blockCreatorV1(genesis, genesisJustification)
        b4 <- blockCreatorV3(b2, HashMap(v1 -> genesis, v2 -> b2, v3 -> b2))
        b5 <- blockCreatorV2(b3, HashMap(v1 -> b3, v2 -> b2, v3 -> genesis))
        b6 <- blockCreatorV1(b4, HashMap(v1 -> b3, v2 -> b2, v3 -> b4))
        b7 <- blockCreatorV3(b5, HashMap(v1 -> b3, v2 -> b5, v3 -> b4))
        b8 <- blockCreatorV2(b6, HashMap(v1 -> b6, v2 -> b5, v3 -> b4))

        dag <- blockDagStorage.getRepresentation

        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance === 1f +- 0.01f)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance === -1f / 6 +- 0.01f)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance === -1f +- 0.01f)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance === -1f / 6 +- 0.01f)
      } yield result
  }
}
