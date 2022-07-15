package coop.rchain.casper.api

import cats.effect.Resource
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper._
import coop.rchain.casper.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.rholang.{BlockRandomSeed, RuntimeManager}
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
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

  val v1                         = generateValidator("Validator One")
  val v2                         = generateValidator("Validator Two")
  val v3                         = generateValidator("Validator Three")
  val v1Bond                     = (v1, 25L)
  val v2Bond                     = (v2, 20L)
  val v3Bond                     = (v3, 15L)
  val bonds                      = Map(v1Bond, v2Bond, v3Bond)
  private val dummyMergeableName = BlockRandomSeed.nonNegativeMergeableTagName("dummy")

  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("block-response-api-test", dummyMergeableName)
  val maxBlockLimit = 50

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

  "getBlocks" should "return all blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
          blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
          blocksResponse <- blockApi.getBlocks(10)
        } yield blocksResponse.right.get.length should be(8)
      }
  }

  it should "return until depth" in withStorage { implicit blockStore => implicit blockDagStorage =>
    runtimeManagerResource.use { implicit runtimeManager =>
      for {
        genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
        blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
        blocksResponse <- blockApi.getBlocks(2)
      } yield blocksResponse.right.get.length should be(3)
    }
  }

  "getBlocksByHeights" should "return blocks between startBlockNumber and endBlockNumber" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
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
}
