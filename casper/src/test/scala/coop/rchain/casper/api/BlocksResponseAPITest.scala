package coop.rchain.casper.api

import cats.Id
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol._
import coop.rchain.casper._
import coop.rchain.casper.helper._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture
    with BlockDagStorageTestFixture {

  implicit val syncId: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId

  implicit val indexedBlockDagStorage = IndexedBlockDagStorage.createWithId(blockDagStorage)

  val v1     = ByteString.copyFromUtf8("Validator One")
  val v2     = ByteString.copyFromUtf8("Validator Two")
  val v3     = ByteString.copyFromUtf8("Validator Three")
  val v1Bond = Bond(v1, 25)
  val v2Bond = Bond(v2, 20)
  val v3Bond = Bond(v3, 15)
  val bonds  = Seq(v1Bond, v2Bond, v3Bond)

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

  implicit val blockStoreEffect = BlockStore[Id]
  implicit val casperEffect: MultiParentCasper[Id] =
    NoOpsCasperEffect[Id](
      HashMap.empty[BlockHash, BlockMessage],
      Estimator.tips[Id](indexedBlockDagStorage.getRepresentation, genesis.blockHash).toIndexedSeq
    )(syncId, blockStoreEffect, indexedBlockDagStorage)
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
