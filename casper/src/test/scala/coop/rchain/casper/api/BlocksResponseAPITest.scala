package coop.rchain.casper.api

import cats.Id
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol._
import coop.rchain.casper.{genesis, _}
import coop.rchain.casper.helper._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  val v1     = generateValidator("Validator One")
  val v2     = generateValidator("Validator Two")
  val v3     = generateValidator("Validator Three")
  val v1Bond = Bond(v1, 25)
  val v2Bond = Bond(v2, 20)
  val v3Bond = Bond(v3, 15)
  val bonds  = Seq(v1Bond, v2Bond, v3Bond)

  "showMainChain" should "return only blocks in the main chain" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
        dag  <- blockDagStorage.getRepresentation
        tips <- Estimator.tips[Task](dag, genesis.blockHash)
        casperEffect <- NoOpsCasperEffect[Task](
                         HashMap.empty[BlockHash, BlockMessage],
                         tips
                       )
        logEff            = new LogStub[Task]
        casperRef         <- MultiParentCasperRef.of[Task]
        _                 <- casperRef.set(casperEffect)
        turanOracleEffect = SafetyOracle.turanOracle[Task](Sync[Task], logEff)
        blocksResponse <- BlockAPI.showMainChain[Task](Int.MaxValue)(
                           Sync[Task],
                           casperRef,
                           logEff,
                           turanOracleEffect,
                           blockStore
                         )
      } yield blocksResponse.length should be(5)
  }

  "showBlocks" should "return all blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
        dag  <- blockDagStorage.getRepresentation
        tips <- Estimator.tips[Task](dag, genesis.blockHash)
        casperEffect <- NoOpsCasperEffect[Task](
                         HashMap.empty[BlockHash, BlockMessage],
                         tips
                       )
        logEff            = new LogStub[Task]
        casperRef         <- MultiParentCasperRef.of[Task]
        _                 <- casperRef.set(casperEffect)
        turanOracleEffect = SafetyOracle.turanOracle[Task](Sync[Task], logEff)
        blocksResponse <- BlockAPI.showBlocks[Task](Int.MaxValue)(
                           Sync[Task],
                           casperRef,
                           logEff,
                           turanOracleEffect,
                           blockStore
                         )
      } yield
        blocksResponse.length should be(8) // TODO: Switch to 4 when we implement block height correctly
  }

  it should "return until depth" in withStorage { implicit blockStore => implicit blockDagStorage =>
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
      dag  <- blockDagStorage.getRepresentation
      tips <- Estimator.tips[Task](dag, genesis.blockHash)
      casperEffect <- NoOpsCasperEffect[Task](
                       HashMap.empty[BlockHash, BlockMessage],
                       tips
                     )
      logEff            = new LogStub[Task]
      casperRef         <- MultiParentCasperRef.of[Task]
      _                 <- casperRef.set(casperEffect)
      turanOracleEffect = SafetyOracle.turanOracle[Task](Sync[Task], logEff)
      blocksResponse <- BlockAPI.showBlocks[Task](2)(
                         Sync[Task],
                         casperRef,
                         logEff,
                         turanOracleEffect,
                         blockStore
                       )
    } yield
      blocksResponse.length should be(2) // TODO: Switch to 3 when we implement block height correctly
  }
}
