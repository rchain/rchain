package coop.rchain.blockstorage

import java.nio.file.StandardOpenOption

import cats.Id
import coop.rchain.shared.PathOps._
import BlockGen.blockElementsGen
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util.Random

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {

  def withDagStorage[R](f: BlockDagStorage[Id] => R): R

  "DAG Storage" should "be able to lookup a stored block" in {
    withDagStorage { dagStorage =>
      forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
        blockElements.foreach(dagStorage.insert)
        blockElements.foreach { b =>
          val dag = dagStorage.getRepresentation
          dag.lookup(b.blockHash) shouldBe Some(BlockMetadata.fromBlock(b))
          dag.latestMessageHash(b.sender) shouldBe Some(b.blockHash)
          dag.latestMessage(b.sender) shouldBe Some(BlockMetadata.fromBlock(b))
        }
        dagStorage.getRepresentation.latestMessageHashes.size shouldBe blockElements.size
        dagStorage.getRepresentation.latestMessages.size shouldBe blockElements.size
        dagStorage.clear()
      }
    }
  }
}

class BlockDagFileStorageTest extends BlockDagStorageTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("rchain-dag-storage-test-")

  override def withDagStorage[R](f: BlockDagStorage[Id] => R): R = {
    val dataDir = mkTmpDir()
    implicit val blockStore = InMemBlockStore.createWithId
    val store = BlockDagFileStorage.createWithId(
      BlockDagFileStorage.Config(
        dataDir.resolve("latest-messsages-data"),
        dataDir.resolve("latest-messsages-checksum"),
        dataDir.resolve("block-metadata-data"),
        dataDir.resolve("block-metadata-checksum")
      )
    )
    try {
      f(store)
    } finally {
      dataDir.recursivelyDelete()
    }
  }

  private def createAtDefaultLocation(
      dataDir: Path,
      maxSizeFactor: Int = 10
  ): BlockDagFileStorage[Id] = {
    implicit val blockStore = InMemBlockStore.createWithId
    BlockDagFileStorage.createWithId(
      BlockDagFileStorage.Config(
        dataDir.resolve("latest-messsages-data"),
        dataDir.resolve("latest-messsages-checksum"),
        dataDir.resolve("block-metadata-data"),
        dataDir.resolve("block-metadata-checksum"),
        maxSizeFactor
      )
    )
  }

  it should "be able to restore state on startup" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir    = mkTmpDir()
      val firstStore = createAtDefaultLocation(dataDir)
      blockElements.foreach(firstStore.insert)
      firstStore.close()
      val secondStore = createAtDefaultLocation(dataDir)
      blockElements.foreach { b =>
        val dag = secondStore.getRepresentation
        dag.latestMessageHash(b.sender) shouldBe Some(b.blockHash)
        dag.lookup(b.blockHash) shouldBe Some(BlockMetadata.fromBlock(b))
      }
      secondStore.close()
      dataDir.recursivelyDelete()
    }
  }

  it should "be able to restore state from the previous two instances" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { firstBlockElements =>
      forAll(blockElementsGen, minSize(0), sizeRange(10)) { secondBlockElements =>
        val dataDir    = mkTmpDir()
        val firstStore = createAtDefaultLocation(dataDir)
        firstBlockElements.foreach(firstStore.insert)
        firstStore.close()
        val secondStore = createAtDefaultLocation(dataDir)
        secondBlockElements.foreach(secondStore.insert)
        secondStore.close()
        val thirdStore = createAtDefaultLocation(dataDir)
        (firstBlockElements ++ secondBlockElements).foreach { b =>
          val dag = thirdStore.getRepresentation
          dag.latestMessageHash(b.sender) shouldBe Some(b.blockHash)
          dag.lookup(b.blockHash) shouldBe Some(BlockMetadata.fromBlock(b))
          dag.latestMessage(b.sender) shouldBe Some(BlockMetadata.fromBlock(b))
        }
        thirdStore.close()
        dataDir.recursivelyDelete()
      }
    }
  }

  it should "be able to restore latest messages on startup with appended 64 garbage bytes" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir    = mkTmpDir()
      val firstStore = createAtDefaultLocation(dataDir)
      blockElements.foreach(firstStore.insert)
      firstStore.close()
      val garbageBytes = Array.fill[Byte](64)(0)
      Random.nextBytes(garbageBytes)
      Files.write(dataDir.resolve("latest-messsages-data"), garbageBytes, StandardOpenOption.APPEND)
      val secondStore = createAtDefaultLocation(dataDir)
      blockElements.foreach { b =>
        val dag = secondStore.getRepresentation
        dag.latestMessageHash(b.sender) shouldBe Some(b.blockHash)
      }
      secondStore.close()
      dataDir.recursivelyDelete()
    }
  }

  it should "be able to handle fully corrupted latest messages data file" in {
    val dataDir      = mkTmpDir()
    val garbageBytes = Array.fill[Byte](789)(0)
    Random.nextBytes(garbageBytes)
    Files.write(dataDir.resolve("latest-messsages-data"), garbageBytes)
    val store = createAtDefaultLocation(dataDir)
    val dag   = store.getRepresentation
    dag.latestMessageHashes.size shouldBe 0
    store.close()
    dataDir.recursivelyDelete()
  }

  it should "be able to restore after squashing latest messages" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir    = mkTmpDir()
      val firstStore = createAtDefaultLocation(dataDir, 2)
      blockElements.foreach(firstStore.insert)
      blockElements.foreach(firstStore.insert)
      blockElements.foreach(firstStore.insert)
      firstStore.close()
      val secondStore = createAtDefaultLocation(dataDir)
      blockElements.foreach { b =>
        val dag = secondStore.getRepresentation
        dag.latestMessageHash(b.sender) shouldBe Some(b.blockHash)
      }
      secondStore.close()
      dataDir.recursivelyDelete()
    }
  }
}
