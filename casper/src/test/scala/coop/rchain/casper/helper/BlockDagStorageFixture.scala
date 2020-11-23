package coop.rchain.casper.helper

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.zip.CRC32

import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{
  BlockDagKeyValueStorage,
  BlockDagStorage,
  IndexedBlockDagStorage
}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.storage.{RNodeKeyValueStoreManager, RocksDbStoreManager}
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{BeforeAndAfter, Suite}

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  val scheduler = Scheduler.fixedPool("block-dag-storage-fixture-scheduler", 4)

  def withGenesis[R](
      context: GenesisContext
  )(f: BlockStore[Task] => IndexedBlockDagStorage[Task] => RuntimeManager[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    val resource = for {
      paths        <- Resources.copyStorage[Task](context.storageDirectory)
      blockStore   <- Resources.mkBlockStoreAt[Task](paths.blockStoreDir)
      storeManager <- Resource.liftF(RocksDbStoreManager[Task](paths.blockDagDir))
      blockDagStorage <- Resource.liftF({
                          implicit val kvm = storeManager
                          BlockDagKeyValueStorage.create[Task]
                        })
      indexedBlockDagStorage <- Resource.liftF(IndexedBlockDagStorage.create[Task](blockDagStorage))
      runtime                <- Resources.mkRuntimeManagerAt[Task](paths.rspaceDir)()
    } yield (blockStore, indexedBlockDagStorage, runtime)

    resource.use[Task, R] { case (b, d, r) => f(b)(d)(r) }.unsafeRunSync
  }

  def withStorage[R](f: BlockStore[Task] => IndexedBlockDagStorage[Task] => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      Sync[Task].delay {
        (BlockDagStorageTestFixture.blockDagStorageDir, BlockDagStorageTestFixture.blockStorageDir)
      }
    } {
      case (blockDagStorageDir, blockStorageDir) =>
        implicit val metrics = new MetricsNOP[Task]()
        implicit val log     = Log.log[Task]
        for {
          blockStore <- BlockDagStorageTestFixture.createBlockStorage[Task](blockStorageDir)
          blockDagStorage <- BlockDagStorageTestFixture.createBlockDagStorage[Task](
                              blockDagStorageDir
                            )
          indexedBlockDagStorage <- IndexedBlockDagStorage.create(blockDagStorage)
          result                 <- f(blockStore)(indexedBlockDagStorage)
        } yield result
    } {
      case (blockDagStorageDir, blockStorageDir) =>
        Sync[Task].delay {
          blockDagStorageDir.recursivelyDelete()
          blockStorageDir.recursivelyDelete()
        }
    }
    testProgram.unsafeRunSync(scheduler)
  }
}

object BlockDagStorageTestFixture {
  def blockDagStorageDir: Path = Files.createTempDirectory("casper-block-dag-storage-test-")
  def blockStorageDir: Path    = Files.createTempDirectory("casper-block-storage-test-")

  def writeInitialLatestMessages(
      latestMessagesData: Path,
      latestMessagesCrc: Path,
      latestMessages: Map[Validator, BlockMessage]
  ): Unit = {
    val data = latestMessages
      .foldLeft(ByteString.EMPTY) {
        case (byteString, (validator, block)) =>
          byteString.concat(validator).concat(block.blockHash)
      }
      .toByteArray
    val crc = new CRC32()
    latestMessages.foreach {
      case (validator, block) =>
        crc.update(validator.concat(block.blockHash).toByteArray)
    }
    val crcByteBuffer = ByteBuffer.allocate(8)
    crcByteBuffer.putLong(crc.getValue)
    Files.write(latestMessagesData, data)
    Files.write(latestMessagesCrc, crcByteBuffer.array())
  }

  val mapSize: Long = 1024L * 1024L * 1024L

  def createBlockStorage[F[_]: Concurrent: Log](
      blockStorageDir: Path
  ): F[BlockStore[F]] =
    for {
      storeManager <- RocksDbStoreManager[F](blockStorageDir)
      blockStore <- {
        implicit val kvm = storeManager
        KeyValueBlockStore[F]()
      }
    } yield blockStore

  def createBlockDagStorage[F[_]: Concurrent](blockDagStorageDir: Path)(
      implicit log: Log[F],
      metrics: Metrics[F]
  ): F[BlockDagStorage[F]] =
    for {
      storeManager <- RocksDbStoreManager[F](blockDagStorageDir)
      blockDagStorage <- {
        implicit val kvm = storeManager
        BlockDagKeyValueStorage.create[F]
      }
    } yield blockDagStorage

}
