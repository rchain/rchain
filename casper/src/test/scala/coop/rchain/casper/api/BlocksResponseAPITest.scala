package coop.rchain.casper.api

import cats.effect.Resource
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.DeployStatus
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryKeyValueStore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture {

  implicit val log: Log[Task]       = new Log.NOPLog[Task]()
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  val v1     = generateValidator("Validator One")
  val v2     = generateValidator("Validator Two")
  val v3     = generateValidator("Validator Three")
  val v1Bond = Bond(v1, 25)
  val v2Bond = Bond(v2, 20)
  val v3Bond = Bond(v3, 15)
  val bonds  = Seq(v1Bond, v2Bond, v3Bond)
  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("block-response-api-test")
  val maxBlockLimit = 50

  val etState: Ref[Task, Map[DeployId, Option[DeployStatus]]] =
    Ref[Task].of(Map.empty[DeployId, Option[DeployStatus]]).runSyncUnsafe()

  private def createDagWith8Blocks(
      implicit blockstore: BlockStore[Task],
      dagstore: BlockDagStorage[Task]
  ) =
    for {
      genesis <- createGenesis[Task](bonds = bonds)
      _       <- dagstore.insert(genesis, invalid = false, approved = true)
      _       <- blockstore.put(genesis)
      b2 <- createBlock[Task](
             Seq(genesis.blockHash),
             genesis,
             v2,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
           )
      b3 <- createBlock[Task](
             Seq(genesis.blockHash),
             genesis,
             v1,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
           )
      b4 <- createBlock[Task](
             Seq(b2.blockHash),
             genesis,
             v3,
             bonds,
             HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
           )
      b5 <- createBlock[Task](
             Seq(b3.blockHash),
             genesis,
             v2,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
           )
      b6 <- createBlock[Task](
             Seq(b4.blockHash),
             genesis,
             v1,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
           )
      b7 <- createBlock[Task](
             Seq(b5.blockHash),
             genesis,
             v3,
             bonds,
             HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
           )
      b8 <- createBlock[Task](
             Seq(b6.blockHash),
             genesis,
             v2,
             bonds,
             HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
           )
    } yield genesis

  "getBlocks" should "return all blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
          blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit, etState)
          blocksResponse <- blockApi.getBlocks(10)
        } yield blocksResponse.right.get.length should be(8)
      }
  }

  it should "return until depth" in withStorage { implicit blockStore => implicit blockDagStorage =>
    runtimeManagerResource.use { implicit runtimeManager =>
      for {
        genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
        blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit, etState)
        blocksResponse <- blockApi.getBlocks(2)
      } yield blocksResponse.right.get.length should be(3)
    }
  }

  "getBlocksByHeights" should "return blocks between startBlockNumber and endBlockNumber" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
          blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit, etState)
          blocksResponse <- blockApi.getBlocksByHeights(2, 5)
          blocks         = blocksResponse.right.get
          _              = blocks.length should be(5)
          _              = blocks.head.blockNumber should be(2)
          _              = blocks.last.blockNumber should be(4)
        } yield ()
      }
  }
}
