package coop.rchain.blockstorage

import java.nio.file.StandardOpenOption

import cats.implicits._
import coop.rchain.shared.PathOps._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import BlockGen._
import cats.effect.Sync
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.rspace.Context
import coop.rchain.shared
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util.Random

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  val scheduler = Scheduler.fixedPool("block-dag-storage-test-scheduler", 4)

  def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R

  "DAG Storage" should "be able to lookup a stored block" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { dagStorage =>
        for {
          _   <- blockElements.traverse_(dagStorage.insert)
          dag <- dagStorage.getRepresentation
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
          _ = blockElementLookups.zip(blockElements).foreach {
            case ((blockMetadata, latestMessageHash, latestMessage), b) =>
              blockMetadata shouldBe Some(BlockMetadata.fromBlock(b))
              latestMessageHash shouldBe Some(b.blockHash)
              latestMessage shouldBe Some(BlockMetadata.fromBlock(b))
          }
          _      = latestMessageHashes.size shouldBe blockElements.size
          result = latestMessages.size shouldBe blockElements.size
        } yield result
      }
    }
  }
}

class BlockDagFileStorageTest extends BlockDagStorageTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("rchain-dag-storage-test-")

  def withDagStorageLocation[R](f: (Path, BlockStore[Task]) => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      Sync[Task].delay {
        (mkTmpDir(), mkTmpDir())
      }
    } {
      case (dagDataDir, blockStoreDataDir) =>
        val blockStore = createBlockStore(blockStoreDataDir)
        for {
          result <- f(dagDataDir, blockStore)
          _      <- blockStore.close()
        } yield result
    } {
      case (dagDataDir, blockStoreDataDir) =>
        Sync[Task].delay {
          dagDataDir.recursivelyDelete()
          blockStoreDataDir.recursivelyDelete()
        }
    }
    testProgram.unsafeRunSync(scheduler)
  }

  override def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R =
    withDagStorageLocation { (dagDataDir, blockStore) =>
      for {
        dagStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
        result     <- f(dagStorage)
        _          <- dagStorage.close()
      } yield result
    }

  private def defaultLatestMessagesLog(dagDataDir: Path): Path =
    dagDataDir.resolve("latest-messsages-data")

  private def defaultLatestMessagesCrc(dagDataDir: Path): Path =
    dagDataDir.resolve("latest-messsages-checksum")

  private def defaultBlockMetadataLog(dagDataDir: Path): Path =
    dagDataDir.resolve("block-metadata-data")

  private def defaultBlockMetadataCrc(dagDataDir: Path): Path =
    dagDataDir.resolve("block-metadata-checksum")

  private def defaultCheckpointsDir(dagDataDir: Path): Path =
    dagDataDir.resolve("checkpoints")

  private def createBlockStore(blockStoreDataDir: Path): BlockStore[Task] = {
    val env              = Context.env(blockStoreDataDir, 100L * 1024L * 1024L * 4096L)
    implicit val metrics = new MetricsNOP[Task]()
    LMDBBlockStore.create[Task](env, blockStoreDataDir)
  }

  private def createAtDefaultLocation(
      dagDataDir: Path,
      maxSizeFactor: Int = 10
  )(implicit blockStore: BlockStore[Task]): Task[BlockDagFileStorage[Task]] = {
    implicit val log     = new shared.Log.NOPLog[Task]()
    implicit val metrics = new MetricsNOP[Task]()
    BlockDagFileStorage.create[Task](
      BlockDagFileStorage.Config(
        defaultLatestMessagesLog(dagDataDir),
        defaultLatestMessagesCrc(dagDataDir),
        defaultBlockMetadataLog(dagDataDir),
        defaultBlockMetadataCrc(dagDataDir),
        defaultCheckpointsDir(dagDataDir),
        maxSizeFactor
      )
    )
  }

  type LookupResult =
    (
        List[
          (
              Option[BlockMetadata],
              Option[BlockHash],
              Option[BlockMetadata],
              Option[Set[BlockHash]],
              Boolean
          )
        ],
        Map[Validator, BlockHash],
        Map[Validator, BlockMetadata],
        Vector[Vector[BlockHash]],
        Vector[Vector[BlockHash]]
    )

  private def lookupElements(
      blockElements: List[BlockMessage],
      storage: BlockDagStorage[Task],
      topoSortStartBlockNumber: Long = 0,
      topoSortTailLength: Int = 5
  ): Task[LookupResult] =
    for {
      dag <- storage.getRepresentation
      list <- blockElements.traverse { b =>
               for {
                 blockMetadata     <- dag.lookup(b.blockHash)
                 latestMessageHash <- dag.latestMessageHash(b.sender)
                 latestMessage     <- dag.latestMessage(b.sender)
                 children          <- dag.children(b.blockHash)
                 contains          <- dag.contains(b.blockHash)
               } yield (blockMetadata, latestMessageHash, latestMessage, children, contains)
             }
      latestMessageHashes <- dag.latestMessageHashes
      latestMessages      <- dag.latestMessages
      topoSort            <- dag.topoSort(topoSortStartBlockNumber)
      topoSortTail        <- dag.topoSortTail(topoSortTailLength)
    } yield (list, latestMessageHashes, latestMessages, topoSort, topoSortTail)

  private def testLookupElementsResult(
      lookupResult: LookupResult,
      blockElements: List[BlockMessage],
      topoSortStartBlockNumber: Long = 0,
      topoSortTailLength: Int = 5
  ): Assertion = {
    val (list, latestMessageHashes, latestMessages, topoSort, topoSortTail) = lookupResult
    val realLatestMessages = blockElements.foldLeft(Map.empty[Validator, BlockMetadata]) {
      case (lm, b) =>
        lm.updated(b.sender, BlockMetadata.fromBlock(b))
    }
    list.zip(blockElements).foreach {
      case ((blockMetadata, latestMessageHash, latestMessage, children, contains), b) =>
        blockMetadata shouldBe Some(BlockMetadata.fromBlock(b))
        latestMessageHash shouldBe realLatestMessages.get(b.sender).map(_.blockHash)
        latestMessage shouldBe realLatestMessages.get(b.sender)
        children shouldBe
          Some(
            blockElements
              .filter(_.header.get.parentsHashList.contains(b.blockHash))
              .map(_.blockHash)
              .toSet
          )
        contains shouldBe true
    }
    latestMessageHashes shouldBe blockElements.map(b => b.sender -> b.blockHash).toMap
    latestMessages shouldBe blockElements.map(b => b.sender      -> BlockMetadata.fromBlock(b)).toMap

    def normalize(topoSort: Vector[Vector[BlockHash]]): Vector[Vector[BlockHash]] =
      if (topoSort.size == 1 && topoSort.head.isEmpty)
        Vector.empty
      else
        topoSort

    val realTopoSort = normalize(Vector(blockElements.map(_.blockHash).toVector))
    topoSort shouldBe realTopoSort.drop(topoSortStartBlockNumber.toInt)
    topoSortTail shouldBe realTopoSort.takeRight(topoSortTailLength)
  }

  it should "be able to restore state on startup" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { (dagDataDir, blockStore) =>
        for {
          firstStorage  <- createAtDefaultLocation(dagDataDir)(blockStore)
          _             <- blockElements.traverse_(firstStorage.insert)
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(result, blockElements)
      }
    }
  }

  it should "be able to restore state from the previous two instances" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { firstBlockElements =>
      forAll(blockElementsGen, minSize(0), sizeRange(10)) { secondBlockElements =>
        withDagStorageLocation { (dagDataDir, blockStore) =>
          for {
            firstStorage  <- createAtDefaultLocation(dagDataDir)(blockStore)
            _             <- firstBlockElements.traverse_(firstStorage.insert)
            _             <- firstStorage.close()
            secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
            _             <- secondBlockElements.traverse_(secondStorage.insert)
            _             <- secondStorage.close()
            thirdStorage  <- createAtDefaultLocation(dagDataDir)(blockStore)
            result        <- lookupElements(firstBlockElements ++ secondBlockElements, thirdStorage)
            _             <- thirdStorage.close()
          } yield testLookupElementsResult(result, firstBlockElements ++ secondBlockElements)
        }
      }
    }
  }

  it should "be able to restore latest messages on startup with appended 64 garbage bytes" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { (dagDataDir, blockStore) =>
        for {
          firstStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
          _            <- blockElements.traverse_(firstStorage.insert)
          _            <- firstStorage.close()
          garbageBytes = Array.fill[Byte](64)(0)
          _            <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
          _ <- Sync[Task].delay {
                Files.write(
                  defaultLatestMessagesLog(dagDataDir),
                  garbageBytes,
                  StandardOpenOption.APPEND
                )
              }
          secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(result, blockElements)
      }
    }
  }

  it should "be able to restore data lookup on startup with appended garbage block metadata" in {
    forAll(blockElementsGen, blockElementGen, minSize(0), sizeRange(10)) {
      (blockElements, garbageBlock) =>
        withDagStorageLocation { (dagDataDir, blockStore) =>
          for {
            firstStorage      <- createAtDefaultLocation(dagDataDir)(blockStore)
            _                 <- blockElements.traverse_(firstStorage.insert)
            _                 <- firstStorage.close()
            garbageByteString = BlockMetadata.fromBlock(garbageBlock).toByteString
            garbageBytes      = garbageByteString.size.toByteString.concat(garbageByteString).toByteArray
            _ <- Sync[Task].delay {
                  Files.write(
                    defaultBlockMetadataLog(dagDataDir),
                    garbageBytes,
                    StandardOpenOption.APPEND
                  )
                }
            secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
            result        <- lookupElements(blockElements, secondStorage)
            _             <- secondStorage.close()
          } yield testLookupElementsResult(result, blockElements)
        }
    }
  }

  it should "be able to handle fully corrupted latest messages log file" in withDagStorageLocation {
    (dagDataDir, blockStore) =>
      val garbageBytes = Array.fill[Byte](789)(0)
      for {
        _                   <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
        _                   <- Sync[Task].delay { Files.write(defaultLatestMessagesLog(dagDataDir), garbageBytes) }
        storage             <- createAtDefaultLocation(dagDataDir)(blockStore)
        dag                 <- storage.getRepresentation
        latestMessageHashes <- dag.latestMessageHashes
        latestMessages      <- dag.latestMessages
        _                   <- storage.close()
        _                   = latestMessageHashes.size shouldBe 0
        result              = latestMessages.size shouldBe 0
      } yield result
  }

  it should "be able to restore after squashing latest messages" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockElements =>
      forAll(blockWithNewHashesGen(blockElements), blockWithNewHashesGen(blockElements)) {
        (secondBlockElements, thirdBlockElements) =>
          withDagStorageLocation { (dagDataDir, blockStore) =>
            for {
              firstStorage  <- createAtDefaultLocation(dagDataDir, 2)(blockStore)
              _             <- blockElements.traverse_(firstStorage.insert)
              _             <- secondBlockElements.traverse_(firstStorage.insert)
              _             <- thirdBlockElements.traverse_(firstStorage.insert)
              _             <- firstStorage.close()
              secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
              result        <- lookupElements(blockElements, secondStorage)
              _             <- secondStorage.close()
            } yield
              testLookupElementsResult(
                result,
                blockElements ++ secondBlockElements ++ thirdBlockElements
              )
          }
      }
    }
  }

  it should "be able to load checkpoints" in {
    forAll(blockElementsGen, minSize(1), sizeRange(2)) { blockElements =>
      withDagStorageLocation { (dagDataDir, blockStore) =>
        for {
          firstStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
          _ <- blockElements.traverse_(
                b => blockStore.put(b.blockHash, b) *> firstStorage.insert(b)
              )
          _ <- firstStorage.close()
          _ <- Sync[Task].delay {
                Files.move(
                  defaultBlockMetadataLog(dagDataDir),
                  defaultCheckpointsDir(dagDataDir).resolve("0-1")
                )
                Files.delete(defaultBlockMetadataCrc(dagDataDir))
              }
          secondStorage <- createAtDefaultLocation(dagDataDir)(blockStore)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield
          testLookupElementsResult(
            result,
            blockElements
          )
      }
    }
  }
}
