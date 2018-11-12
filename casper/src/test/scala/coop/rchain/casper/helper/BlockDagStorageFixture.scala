package coop.rchain.casper.helper

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.zip.CRC32

import cats.Id
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.{
  BlockDagFileStorage,
  BlockDagStorage,
  IndexedBlockDagStorage,
  LMDBBlockStore
}
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Suite}
import coop.rchain.shared.PathOps.RichPath

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  def withBlockDagStorage[R](f: BlockDagStorage[Id] => R): R = {
    val blockDagDir   = BlockDagStorageTestFixture.dir
    val blockStoreDir = BlockStoreTestFixture.dbDir
    val store         = BlockDagStorageTestFixture.create(blockDagDir, blockStoreDir)
    try {
      f(store)
    } finally {
      blockDagDir.recursivelyDelete()
      blockStoreDir.recursivelyDelete()
    }
  }

  def withIndexedBlockDagStorage[R](f: IndexedBlockDagStorage[Id] => R): R =
    withBlockDagStorage { blockDagStorage =>
      f(IndexedBlockDagStorage.createWithId(blockDagStorage))
    }
}

object BlockDagStorageTestFixture {
  def dir: Path = Files.createTempDirectory("casper-block-dag-storage-test-")

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

  def create(blockDagDir: Path, blockStoreDir: Path): IndexedBlockDagStorage[Id] = {
    implicit val blockStore = BlockStoreTestFixture.create(blockStoreDir)
    IndexedBlockDagStorage.createWithId(
      BlockDagFileStorage.createWithId(
        BlockDagFileStorage.Config(
          dir.resolve("latest-messages-data"),
          dir.resolve("latest-messages-crc"),
          dir.resolve("block-metadata-data"),
          dir.resolve("block-metadata-crc"),
          dir.resolve("checkpoints")
        )
      )
    )
  }
}

trait BlockDagStorageTestFixture extends BeforeAndAfterAll { self: Suite =>

  val blockDagStorageDir = BlockDagStorageTestFixture.dir
  val blockStorageDir    = BlockStoreTestFixture.dbDir

  implicit val blockDagStorage =
    BlockDagStorageTestFixture.create(blockDagStorageDir, blockStorageDir)

  override def afterAll(): Unit = {
    blockDagStorage.close()
    blockDagStorageDir.recursivelyDelete()
    blockStorageDir.recursivelyDelete()
  }
}
