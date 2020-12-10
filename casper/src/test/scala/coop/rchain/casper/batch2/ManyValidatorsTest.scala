package coop.rchain.casper.batch2

import cats.Monad
import cats.effect.{Resource, Sync}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.IndexedBlockDagStorage
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{Estimator, SafetyOracle}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.CanBlock
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import scala.concurrent.duration._
import scala.util.Random

class ManyValidatorsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture
    with UnlimitedParentsEstimatorFixture {
  "Get blocks" should "be processed quickly for a node with 300 validators" in {
    val blockDagStorageDir = BlockDagStorageTestFixture.blockDagStorageDir
    val blockStoreDir      = BlockDagStorageTestFixture.blockStorageDir
    val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
      mkRuntimeManager("many-validators-test")

    val bonds = Seq
      .fill(300) {
        val array = Array.ofDim[Byte](Validator.Length)
        Random.nextBytes(array)
        ByteString.copyFrom(array)
      }
      .map(Bond(_, 10))
    val v1 = bonds(0).validator

    val testProgram = runtimeManagerResource.use { implicit runtimeManager =>
      for {
        blockStore <- BlockDagStorageTestFixture.createBlockStorage[Task](blockStoreDir)
        blockDagStorage <- BlockDagStorageTestFixture.createBlockDagStorage[Task](
                            blockDagStorageDir
                          )
        indexedBlockDagStorage <- IndexedBlockDagStorage.create(blockDagStorage)
        genesis <- createGenesis[Task](bonds = bonds)(
                    Monad[Task],
                    Time[Task],
                    blockStore,
                    indexedBlockDagStorage
                  )
        b <- createBlock[Task](Seq(genesis.blockHash), genesis, v1, bonds, bonds.map {
              case Bond(validator, _) => validator -> genesis.blockHash
            }.toMap)(Monad[Task], Time[Task], blockStore, indexedBlockDagStorage)
        _                     <- indexedBlockDagStorage.close()
        initialLatestMessages = bonds.map { case Bond(validator, _) => validator -> b }.toMap
        _ <- Sync[Task].delay {
              BlockDagStorageTestFixture.writeInitialLatestMessages(
                blockDagStorageDir.resolve("latest-messages-data"),
                blockDagStorageDir.resolve("latest-messages-crc"),
                initialLatestMessages
              )
            }
        newBlockDagStorage <- BlockDagStorageTestFixture.createBlockDagStorage[Task](
                               blockDagStorageDir
                             )
        newIndexedBlockDagStorage <- IndexedBlockDagStorage.create(newBlockDagStorage)
        dag                       <- newIndexedBlockDagStorage.getRepresentation
        tips                      <- Estimator[Task].tips(dag, genesis)
        casperEffect <- NoOpsCasperEffect[Task](
                         HashMap.empty[BlockHash, BlockMessage],
                         tips.tips
                       )(Sync[Task], blockStore, newIndexedBlockDagStorage, runtimeManager)
        logEff             = new LogStub[Task]
        engine             = new EngineWithCasper[Task](casperEffect)
        engineCell         <- Cell.mvarCell[Task, Engine[Task]](engine)
        cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
        result <- BlockAPI.getBlocks[Task](Int.MaxValue, 50)(
                   Sync[Task],
                   engineCell,
                   logEff,
                   cliqueOracleEffect,
                   blockStore
                 )
      } yield result
    }
    testProgram.runSyncUnsafe(1.minute)(scheduler, CanBlock.permit)
  }
}
