package coop.rchain.casper.api

import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper._
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log
import monix.eval.Task
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends AnyFlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture {

  implicit val log: Log[Task]       = new Log.NOPLog[Task]()
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  val v1: Validator               = generateValidator("Validator One")
  val v2: Validator               = generateValidator("Validator Two")
  val v3: Validator               = generateValidator("Validator Three")
  val v1Bond: (Validator, Long)   = (v1, 25L)
  val v2Bond: (Validator, Long)   = (v2, 20L)
  val v3Bond: (Validator, Long)   = (v3, 15L)
  val bonds: Map[Validator, Long] = Map(v1Bond, v2Bond, v3Bond)
  val maxBlockLimit               = 50

  private def createDagWith8Blocks(
      implicit blockstore: BlockStore[Task],
      dagstore: BlockDagStorage[Task]
  ) =
    for {
      genesis <- createGenesis[Task](bonds = bonds)
      b2 <- createBlock[Task](
             v2,
             bonds,
             Seq(genesis.blockHash)
           )
      b3 <- createBlock[Task](
             v1,
             bonds,
             Seq(genesis.blockHash)
           )
      b4 <- createBlock[Task](
             v3,
             bonds,
             Seq(genesis.blockHash, b2.blockHash)
           )
      b5 <- createBlock[Task](
             v2,
             bonds,
             Seq(b3.blockHash, b2.blockHash, genesis.blockHash)
           )
      b6 <- createBlock[Task](
             v1,
             bonds,
             Seq(b3.blockHash, b2.blockHash, b4.blockHash)
           )
      b7 <- createBlock[Task](
             v3,
             bonds,
             Seq(b3.blockHash, b5.blockHash, b4.blockHash)
           )
      b8 <- createBlock[Task](
             v2,
             bonds,
             Seq(b6.blockHash, b5.blockHash, b4.blockHash)
           )
    } yield genesis

  "getBlocks" should "return all blocks" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocks(10)
    } yield blocksResponse.right.get.length should be(8)
  }

  it should "return until depth" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocks(2)
    } yield blocksResponse.right.get.length should be(3)
  }

  "getBlocksByHeights" should "return blocks between startBlockNumber and endBlockNumber" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = Mocks.create[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocksByHeights(2, 5)
      blocks         = blocksResponse.right.get
      _              = blocks.length should be(5)
      _              = blocks.head.blockNumber should be(2)
      _              = blocks.last.blockNumber should be(4)
    } yield ()
  }
}
