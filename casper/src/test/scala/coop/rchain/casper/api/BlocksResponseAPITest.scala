package coop.rchain.casper.api

import cats.{Applicative, Id}
import cats.effect.Sync
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper._
import coop.rchain.casper.helper._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.p2p.EffectsTestInstances.LogStub
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import coop.rchain.shared.Time

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {

  implicit val syncId: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId

  val initState = IndexedBlockDag.empty.withOffset(1L)
  val v1        = ByteString.copyFromUtf8("Validator One")
  val v2        = ByteString.copyFromUtf8("Validator Two")
  val v3        = ByteString.copyFromUtf8("Validator Three")
  val v1Bond    = Bond(v1, 25)
  val v2Bond    = Bond(v2, 20)
  val v3Bond    = Bond(v3, 15)
  val bonds     = Seq(v1Bond, v2Bond, v3Bond)

  implicit def timeState[A]: Time[StateWithChain] =
    Time.stateTTime[IndexedBlockDag, Id](syncId, timeEff)

  val createChain =
    for {
      genesis <- createBlock[StateWithChain](Seq(), ByteString.EMPTY, bonds)
      b2 <- createBlock[StateWithChain](
             Seq(genesis.blockHash),
             v2,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
           )
      b3 <- createBlock[StateWithChain](
             Seq(genesis.blockHash),
             v1,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
           )
      b4 <- createBlock[StateWithChain](
             Seq(b2.blockHash),
             v3,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
           )
      b5 <- createBlock[StateWithChain](
             Seq(b3.blockHash),
             v2,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
           )
      b6 <- createBlock[StateWithChain](
             Seq(b4.blockHash),
             v1,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
           )
      b7 <- createBlock[StateWithChain](
             Seq(b5.blockHash),
             v3,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
           )
      b8 <- createBlock[StateWithChain](
             Seq(b6.blockHash),
             v2,
             bonds,
             HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
           )
    } yield b8

  val chain: IndexedBlockDag = createChain.runS(initState)
  val genesis                = chain.idToBlocks(1)

  implicit val blockStoreEffect = BlockStore[Id]
  implicit val casperEffect: MultiParentCasper[Id] =
    NoOpsCasperEffect[Id](
      HashMap.empty[BlockHash, BlockMessage],
      Estimator.tips[Id](chain, genesis.blockHash),
      chain
    )(syncId, blockStoreEffect)
  implicit val logEff = new LogStub[Id]
  implicit val casperRef = {
    val tmp = MultiParentCasperRef.of[Id]
    tmp.set(casperEffect)
    tmp
  }
  implicit val turanOracleEffect: SafetyOracle[Id] = SafetyOracle.turanOracle[Id]

  "showMainChain" should "return only blocks in the main chain" in {
    val blocksResponse =
      BlockAPI.showMainChain[Id](Int.MaxValue)(
        syncId,
        casperRef,
        logEff,
        turanOracleEffect,
        blockStore
      )
    blocksResponse.length should be(5)
  }

  "showBlocks" should "return all blocks" in {
    val blocksResponse =
      BlockAPI.showBlocks[Id](Int.MaxValue)(
        syncId,
        casperRef,
        logEff,
        turanOracleEffect,
        blockStore
      )
    blocksResponse.length should be(8) // TODO: Switch to 4 when we implement block height correctly
  }

  it should "return until depth" in {
    val blocksResponse =
      BlockAPI.showBlocks[Id](2)(
        syncId,
        casperRef,
        logEff,
        turanOracleEffect,
        blockStore
      )
    blocksResponse.length should be(2) // TODO: Switch to 3 when we implement block height correctly
  }
}
