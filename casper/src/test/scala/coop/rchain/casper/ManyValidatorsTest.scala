package coop.rchain.casper

import cats.{Id, Monad}
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.catscontrib.effect.implicits.syncId
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.shared.Time
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class ManyValidatorsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  "Show blocks" should "be processed quickly for a node with 300 validators" in {
    val blockDagStorageDir = BlockDagStorageTestFixture.dir
    val blockStoreDir      = BlockStoreTestFixture.dbDir
    val blockDagStorage    = BlockDagStorageTestFixture.create(blockDagStorageDir, blockStoreDir)
    val bonds              = Seq.fill(300)(ByteString.copyFromUtf8(Random.nextString(10))).map(Bond(_, 10))
    val v1                 = bonds(0).validator

    val genesis = createBlock[Id](Seq(), ByteString.EMPTY, bonds)(
      Monad[Id],
      Time[Id],
      BlockStore[Id],
      blockDagStorage
    )
    val b = createBlock[Id](Seq(genesis.blockHash), v1, bonds, bonds.map {
      case Bond(validator, _) => validator -> genesis.blockHash
    }.toMap)(Monad[Id], Time[Id], BlockStore[Id], blockDagStorage)

    blockDagStorage.close()

    val initialLatestMessages =
      bonds.map { case Bond(validator, _) => validator -> b }.toMap
    BlockDagStorageTestFixture.writeInitialLatestMessages(
      blockDagStorageDir.resolve("data"),
      blockDagStorageDir.resolve("checksum"),
      initialLatestMessages
    )
    val newBlockDagStorage = BlockDagStorageTestFixture.create(blockDagStorageDir, blockStoreDir)

    implicit val casperEffect: MultiParentCasper[Id] =
      NoOpsCasperEffect[Id](
        HashMap.empty[BlockHash, BlockMessage],
        Estimator.tips[Id](newBlockDagStorage.getRepresentation, genesis.blockHash).toIndexedSeq
      )(syncId, blockStore, newBlockDagStorage)
    implicit val logEff = new LogStub[Id]
    implicit val casperRef = {
      val tmp = MultiParentCasperRef.of[Id]
      tmp.set(casperEffect)
      tmp
    }
    implicit val turanOracleEffect: SafetyOracle[Id] = SafetyOracle.turanOracle[Id]
    Await.result(Future(BlockAPI.showBlocks[Id](Int.MaxValue)), 1 minute)
  }
}
