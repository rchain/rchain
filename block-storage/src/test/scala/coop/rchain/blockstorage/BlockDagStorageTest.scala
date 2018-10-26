package coop.rchain.blockstorage

import java.nio.file.StandardOpenOption

import cats.implicits._
import coop.rchain.shared.PathOps._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import BlockGen.blockElementsGen
import cats.effect.Sync
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.shared
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util.Random

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  val scheduler = Scheduler.fixedPool("block-dag-storage-test-scheduler", 4)

  def withDagStorage[R](f: Task[BlockDagStorage[Task]] => R): R

  "DAG Storage" should "be able to lookup a stored block" in {
    withDagStorage { dagStorageTask =>
      forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
        val testProgram = for {
          dagStorage <- dagStorageTask
          _          <- blockElements.traverse_(dagStorage.insert)
          dag        <- dagStorage.getRepresentation
          blockElementLookups <- blockElements.traverse { b =>
                                  for {
                                    blockMetadata     <- dag.lookup(b.blockHash)
                                    latestMessageHash <- dag.latestMessageHash(b.sender)
                                    latestMessage     <- dag.latestMessage(b.sender)
                                  } yield (blockMetadata, latestMessageHash, latestMessage)
                                }
          latestMessageHashes <- dag.latestMessageHashes
          latestMessages      <- dag.latestMessages
          _                   <- dagStorage.clear()
        } yield (blockElementLookups, latestMessageHashes, latestMessages)
        val (blockElementLookups, latestMessageHashes, latestMessages) =
          testProgram.unsafeRunSync(scheduler)
        blockElementLookups.zip(blockElements).foreach {
          case ((blockMetadata, latestMessageHash, latestMessage), b) =>
            blockMetadata shouldBe Some(BlockMetadata.fromBlock(b))
            latestMessageHash shouldBe Some(b.blockHash)
            latestMessage shouldBe Some(BlockMetadata.fromBlock(b))
        }
        latestMessageHashes.size shouldBe blockElements.size
        latestMessages.size shouldBe blockElements.size
      }
    }
  }
}

class BlockDagFileStorageTest extends BlockDagStorageTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("rchain-dag-storage-test-")

  override def withDagStorage[R](f: Task[BlockDagStorage[Task]] => R): R = {
    val dataDir   = mkTmpDir()
    val storeTask = createAtDefaultLocation(dataDir)
    try {
      f(storeTask)
    } finally {
      dataDir.recursivelyDelete()
    }
  }

  private def defaultLatestMessagesLog(dataDir: Path): Path =
    dataDir.resolve("latest-messsages-data")

  private def defaultLatestMessagesCrc(dataDir: Path): Path =
    dataDir.resolve("latest-messsages-checksum")

  private def defaultBlockMetadataLog(dataDir: Path): Path =
    dataDir.resolve("block-metadata-data")

  private def defaultBlockMetadataCrc(dataDir: Path): Path =
    dataDir.resolve("block-metadata-checksum")

  private def defaultCheckpointsDir(dataDir: Path): Path =
    dataDir.resolve("checkpoints")

  private def createAtDefaultLocation(
      dataDir: Path,
      maxSizeFactor: Int = 10
  ): Task[BlockDagFileStorage[Task]] = {
    implicit val log        = new shared.Log.NOPLog[Task]()
    implicit val metrics    = new MetricsNOP[Task]()
    implicit val refF       = Ref.unsafe[Task, Map[BlockHash, BlockMessage]](Map.empty)
    implicit val blockStore = InMemBlockStore.create[Task]
    BlockDagFileStorage.create[Task](
      BlockDagFileStorage.Config(
        dataDir.resolve("latest-messsages-data"),
        dataDir.resolve("latest-messsages-checksum"),
        dataDir.resolve("block-metadata-data"),
        dataDir.resolve("block-metadata-checksum"),
        dataDir.resolve("checkpoints"),
        maxSizeFactor
      )
    )
  }

  type LookupResult = List[(Option[BlockMetadata], Option[BlockHash], Option[BlockMetadata])]

  private def lookupElements(
      blockElements: List[BlockMessage],
      storage: BlockDagStorage[Task]
  ): Task[LookupResult] =
    for {
      dag <- storage.getRepresentation
      result <- blockElements.traverse { b =>
                 for {
                   blockMetadata     <- dag.lookup(b.blockHash)
                   latestMessageHash <- dag.latestMessageHash(b.sender)
                   latestMessage     <- dag.latestMessage(b.sender)
                 } yield (blockMetadata, latestMessageHash, latestMessage)
               }
    } yield result

  private def testLookupElementsResult(
      lookupResult: LookupResult,
      blockElements: List[BlockMessage]
  ): Unit =
    lookupResult.zip(blockElements).foreach {
      case ((blockMetadata, latestMessageHash, latestMessage), b) =>
        blockMetadata shouldBe Some(BlockMetadata.fromBlock(b))
        latestMessageHash shouldBe Some(b.blockHash)
        latestMessage shouldBe Some(BlockMetadata.fromBlock(b))
    }

  it should "be able to restore state on startup" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir = mkTmpDir()
      val testProgram = for {
        firstStorage  <- createAtDefaultLocation(dataDir)
        _             <- blockElements.traverse_(firstStorage.insert)
        _             <- firstStorage.close()
        secondStorage <- createAtDefaultLocation(dataDir)
        result        <- lookupElements(blockElements, secondStorage)
        _             <- secondStorage.close()
      } yield result
      val blockElementLookups = testProgram.unsafeRunSync(scheduler)
      testLookupElementsResult(blockElementLookups, blockElements)
    }
  }

  it should "be able to restore state from the previous two instances" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { firstBlockElements =>
      forAll(blockElementsGen, minSize(0), sizeRange(10)) { secondBlockElements =>
        val dataDir = mkTmpDir()
        val testProgram = for {
          firstStorage  <- createAtDefaultLocation(dataDir)
          _             <- firstBlockElements.traverse_(firstStorage.insert)
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dataDir)
          _             <- secondBlockElements.traverse_(secondStorage.insert)
          _             <- secondStorage.close()
          thirdStorage  <- createAtDefaultLocation(dataDir)
          result        <- lookupElements(firstBlockElements ++ secondBlockElements, thirdStorage)
          _             <- thirdStorage.close()
        } yield result
        val blockElementLookups = testProgram.unsafeRunSync(scheduler)
        testLookupElementsResult(blockElementLookups, firstBlockElements ++ secondBlockElements)
      }
    }
  }

  it should "be able to restore latest messages on startup with appended 64 garbage bytes" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir = mkTmpDir()
      val testProgram = for {
        firstStorage <- createAtDefaultLocation(dataDir)
        _            <- blockElements.traverse_(firstStorage.insert)
        _            <- firstStorage.close()
        garbageBytes = Array.fill[Byte](64)(0)
        _            <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
        _ <- Sync[Task].delay {
              Files.write(
                defaultLatestMessagesLog(dataDir),
                garbageBytes,
                StandardOpenOption.APPEND
              )
            }
        secondStorage <- createAtDefaultLocation(dataDir)
        result        <- lookupElements(blockElements, secondStorage)
        _             <- secondStorage.close()
      } yield result
      val blockElementLookups = testProgram.unsafeRunSync(scheduler)
      testLookupElementsResult(blockElementLookups, blockElements)
    }
  }

  it should "be able to handle fully corrupted latest messages data file" in {
    val dataDir      = mkTmpDir()
    val garbageBytes = Array.fill[Byte](789)(0)
    val testProgram = for {
      _                   <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
      _                   <- Sync[Task].delay { Files.write(defaultLatestMessagesLog(dataDir), garbageBytes) }
      storage             <- createAtDefaultLocation(dataDir)
      dag                 <- storage.getRepresentation
      latestMessageHashes <- dag.latestMessageHashes
      latestMessages      <- dag.latestMessages
      _                   <- storage.close()
    } yield (latestMessageHashes, latestMessages)
    val (latestMessageHashes, latestMessages) = testProgram.unsafeRunSync(scheduler)
    latestMessageHashes.size shouldBe 0
    latestMessages.size shouldBe 0
  }

  it should "be able to restore after squashing latest messages" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      val dataDir = mkTmpDir()
      val testProgram = for {
        firstStorage  <- createAtDefaultLocation(dataDir, 2)
        _             <- blockElements.traverse_(firstStorage.insert)
        _             <- blockElements.traverse_(firstStorage.insert)
        _             <- blockElements.traverse_(firstStorage.insert)
        _             <- firstStorage.close()
        secondStorage <- createAtDefaultLocation(dataDir)
        result        <- lookupElements(blockElements, secondStorage)
        _             <- secondStorage.close()
      } yield result
      val blockElementLookups = testProgram.unsafeRunSync(scheduler)
      testLookupElementsResult(blockElementLookups, blockElements)
    }
  }

  it should "be able to load checkpoints" in {
    forAll(blockElementsGen, minSize(1), sizeRange(2)) { blockElements =>
      val dataDir = mkTmpDir()
      val testProgram = for {
        firstStorage <- createAtDefaultLocation(dataDir, 2)
        _            <- blockElements.traverse_(firstStorage.insert)
        _            <- firstStorage.close()
        _ <- Sync[Task].delay {
              Files.move(defaultBlockMetadataLog(dataDir),
                         defaultCheckpointsDir(dataDir).resolve("0-1"))
              Files.delete(defaultBlockMetadataCrc(dataDir))
            }
        secondStorage <- createAtDefaultLocation(dataDir)
        dag           <- secondStorage.getRepresentation
        result        <- dag.topoSort(0L)
        _             <- secondStorage.close()
      } yield result
      val topoSort = testProgram.unsafeRunSync(scheduler)
      topoSort shouldBe Vector(blockElements.map(_.blockHash).toVector)
    }
  }
}
