package coop.rchain.blockstorage.dag

import java.nio.file.StandardOpenOption

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.LatestMessagesLogIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.codecs._

import scala.util.Random

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  val scheduler = Scheduler.fixedPool("block-dag-storage-test-scheduler", 4)

  def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R

  val genesis = BlockMessage
    .from(
      BlockMessageProto()
        .withHeader(HeaderProto())
        .withBody(BodyProto().withState(RChainStateProto()))
    )
    .right
    .get

  "DAG Storage" should "be able to lookup a stored block" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { dagStorage =>
        for {
          _   <- blockElements.traverse_(dagStorage.insert(_, genesis, false))
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
          _ = blockElementLookups.zip(blockElements).foreach {
            case ((blockMetadata, latestMessageHash, latestMessage), b) =>
              blockMetadata shouldBe Some(BlockMetadata.fromBlock(b, false))
              latestMessageHash shouldBe Some(b.blockHash)
              latestMessage shouldBe Some(BlockMetadata.fromBlock(b, false))
          }
          _      = latestMessageHashes.size shouldBe blockElements.size
          result = latestMessages.size shouldBe blockElements.size
        } yield result
      }
    }
  }

  it should "be able to handle checking if contains a block with empty hash" in {
    withDagStorage { dagStorage =>
      for {
        dag        <- dagStorage.getRepresentation
        ifContains <- dag.contains(ByteString.EMPTY)
      } yield ifContains shouldBe false
    }
  }
}

class BlockDagFileStorageTest extends BlockDagStorageTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("rchain-dag-storage-test-")

  implicit val raiseIOError: RaiseIOError[Task] = IOError.raiseIOErrorThroughSync[Task]

  def withDagStorageLocation[R](f: Path => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      Sync[Task].delay {
        mkTmpDir()
      }
    } { dagDataDir =>
      f(dagDataDir)
    } { dagDataDir =>
      Sync[Task].delay {
        dagDataDir.recursivelyDelete()
      }
    }
    testProgram.unsafeRunSync(scheduler)
  }

  override def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R =
    withDagStorageLocation { dagDataDir =>
      for {
        dagStorage <- createAtDefaultLocation(dagDataDir)
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

  private def defaultEquivocationsTrackerLog(dagDataDir: Path): Path =
    dagDataDir.resolve("equivocations-tracker-data")

  private def defaultEquivocationsTrackerCrc(dagDataDir: Path): Path =
    dagDataDir.resolve("equivocations-tracker-checksum")

  private def defaultInvalidBlocksLog(dagDataDir: Path): Path =
    dagDataDir.resolve("invalid-blocks-data")

  private def defaultInvalidBlocksCrc(dagDataDir: Path): Path =
    dagDataDir.resolve("invalid-blocks-checksum")

  private def defaultBlockHasesByDeploy(dagDataDir: Path): Path =
    dagDataDir.resolve("block-hashes-by-deploy")

  private def defaultBlockHasesByDeployCrc(dagDataDir: Path): Path =
    dagDataDir.resolve("block-hashes-by-deploy-checksum")

  private def defaultCheckpointsDir(dagDataDir: Path): Path =
    dagDataDir.resolve("checkpoints")

  private def defaultBlockNumberIndex(dagDataDir: Path): Path =
    dagDataDir.resolve("block-number-index")

  private def createAtDefaultLocation(
      dagDataDir: Path,
      maxSizeFactor: Int = 10
  ): Task[BlockDagFileStorage[Task]] = {
    implicit val log     = new shared.Log.NOPLog[Task]()
    implicit val metrics = new Metrics.MetricsNOP[Task]
    BlockDagFileStorage.create[Task](
      BlockDagFileStorage.Config(
        defaultLatestMessagesLog(dagDataDir),
        defaultLatestMessagesCrc(dagDataDir),
        defaultBlockMetadataLog(dagDataDir),
        defaultBlockMetadataCrc(dagDataDir),
        defaultEquivocationsTrackerLog(dagDataDir),
        defaultEquivocationsTrackerCrc(dagDataDir),
        defaultInvalidBlocksLog(dagDataDir),
        defaultInvalidBlocksCrc(dagDataDir),
        defaultBlockHasesByDeploy(dagDataDir),
        defaultBlockHasesByDeployCrc(dagDataDir),
        defaultCheckpointsDir(dagDataDir),
        defaultBlockNumberIndex(dagDataDir),
        100L * 1024L * 1024L * 4096L,
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
  ): Unit = {
    val (list, latestMessageHashes, latestMessages, topoSort, topoSortTail) = lookupResult
    val realLatestMessages = blockElements.foldLeft(Map.empty[Validator, BlockMetadata]) {
      case (lm, b) =>
        // Ignore empty sender for genesis block
        if (b.sender != ByteString.EMPTY)
          lm.updated(b.sender, BlockMetadata.fromBlock(b, false))
        else
          lm
    }
    list.zip(blockElements).foreach {
      case ((blockMetadata, latestMessageHash, latestMessage, children, contains), b) =>
        blockMetadata shouldBe Some(BlockMetadata.fromBlock(b, false))
        latestMessageHash shouldBe realLatestMessages.get(b.sender).map(_.blockHash)
        latestMessage shouldBe realLatestMessages.get(b.sender)
        children shouldBe
          Some(
            blockElements
              .filter(_.header.parentsHashList.contains(b.blockHash))
              .map(_.blockHash)
              .toSet
          )
        contains shouldBe true
    }
    latestMessageHashes shouldBe realLatestMessages.mapValues(_.blockHash)
    latestMessages shouldBe realLatestMessages

    def normalize(topoSort: Vector[Vector[BlockHash]]): Vector[Vector[BlockHash]] =
      if (topoSort.size == 1 && topoSort.head.isEmpty)
        Vector.empty
      else
        topoSort

    val realTopoSort = normalize(Vector(blockElements.map(_.blockHash).toVector))
    for ((topoSortLevel, realTopoSortLevel) <- topoSort.zipAll(
                                                realTopoSort,
                                                Vector.empty,
                                                Vector.empty
                                              )) {
      topoSortLevel.toSet shouldBe realTopoSortLevel.toSet
    }
    for ((topoSortLevel, realTopoSortLevel) <- topoSortTail.zipAll(
                                                realTopoSort.takeRight(topoSortTailLength),
                                                Vector.empty,
                                                Vector.empty
                                              )) {
      topoSortLevel.toSet shouldBe realTopoSortLevel.toSet
    }
  }

  it should "be able to restore state on startup" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage  <- createAtDefaultLocation(dagDataDir)
          _             <- blockElements.traverse_(firstStorage.insert(_, genesis, false))
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dagDataDir)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(result, blockElements)
      }
    }
  }

  it should "be able to restore latest messages with genesis with empty sender field" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      val blockElementsWithGenesis = blockElements match {
        case x :: xs =>
          val genesis = x.copy(sender = ByteString.EMPTY)
          genesis :: xs
        case Nil =>
          Nil
      }
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage  <- createAtDefaultLocation(dagDataDir)
          _             <- blockElementsWithGenesis.traverse_(firstStorage.insert(_, genesis, false))
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dagDataDir)
          result        <- lookupElements(blockElementsWithGenesis, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(result, blockElementsWithGenesis)
      }
    }
  }

  it should "be able to restore state from the previous two instances" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { firstBlockElements =>
      forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { secondBlockElements =>
        withDagStorageLocation { dagDataDir =>
          for {
            firstStorage  <- createAtDefaultLocation(dagDataDir)
            _             <- firstBlockElements.traverse_(firstStorage.insert(_, genesis, false))
            _             <- firstStorage.close()
            secondStorage <- createAtDefaultLocation(dagDataDir)
            _             <- secondBlockElements.traverse_(secondStorage.insert(_, genesis, false))
            _             <- secondStorage.close()
            thirdStorage  <- createAtDefaultLocation(dagDataDir)
            result        <- lookupElements(firstBlockElements ++ secondBlockElements, thirdStorage)
            _             <- thirdStorage.close()
          } yield testLookupElementsResult(result, firstBlockElements ++ secondBlockElements)
        }
      }
    }
  }

  it should "be able to restore latest messages on startup with appended 64 garbage bytes" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage <- createAtDefaultLocation(dagDataDir)
          _            <- blockElements.traverse_(firstStorage.insert(_, genesis, false))
          _            <- firstStorage.close()
          garbageBytes = Array.fill[Byte](Validator.Length + BlockHash.Length)(0)
          _            <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
          _ <- Sync[Task].delay {
                Files.write(
                  defaultLatestMessagesLog(dagDataDir),
                  garbageBytes,
                  StandardOpenOption.APPEND
                )
              }
          secondStorage <- createAtDefaultLocation(dagDataDir)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(result, blockElements)
      }
    }
  }

  it should "be able to restore data lookup on startup with appended garbage block metadata" in {
    forAll(blockElementsWithParentsGen, blockElementGen, minSize(0), sizeRange(10)) {
      (blockElements, garbageBlock) =>
        withDagStorageLocation { dagDataDir =>
          for {
            firstStorage                  <- createAtDefaultLocation(dagDataDir)
            _                             <- blockElements.traverse_(firstStorage.insert(_, genesis, invalid = false))
            _                             <- firstStorage.close()
            garbageBlockMetadata          = BlockMetadata.fromBlock(garbageBlock, invalid = false)
            keyValueCodec                 = (codecBlockHash ~ codecBlockMetadata)
            blockHashBlockMetadataPair    = (garbageBlockMetadata.blockHash, garbageBlockMetadata)
            garbageBlockMetadataBitVector = keyValueCodec.encode(blockHashBlockMetadataPair).get
            garbageBytes                  = garbageBlockMetadataBitVector.toByteArray
            _ <- Sync[Task].delay {
                  Files.write(
                    defaultBlockMetadataLog(dagDataDir),
                    garbageBytes,
                    StandardOpenOption.APPEND
                  )
                }
            secondStorage <- createAtDefaultLocation(dagDataDir)
            result        <- lookupElements(blockElements, secondStorage)
            _             <- secondStorage.close()
          } yield testLookupElementsResult(result, blockElements)
        }
    }
  }

  it should "fail at fully corrupted latest messages log file" in withDagStorageLocation {
    dagDataDir =>
      val garbageBytes = Array.fill[Byte](789)(0)
      for {
        _              <- Sync[Task].delay { Random.nextBytes(garbageBytes) }
        _              <- Sync[Task].delay { Files.write(defaultLatestMessagesLog(dagDataDir), garbageBytes) }
        storageAttempt <- createAtDefaultLocation(dagDataDir).attempt
      } yield storageAttempt.left.value shouldBe LatestMessagesLogIsCorrupted
  }

  it should "be able to restore after squashing latest messages" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      forAll(blockWithNewHashesGen(blockElements), blockWithNewHashesGen(blockElements)) {
        (secondBlockElements, thirdBlockElements) =>
          withDagStorageLocation { dagDataDir =>
            for {
              firstStorage  <- createAtDefaultLocation(dagDataDir, 2)
              _             <- blockElements.traverse_(firstStorage.insert(_, genesis, false))
              _             <- secondBlockElements.traverse_(firstStorage.insert(_, genesis, false))
              _             <- thirdBlockElements.traverse_(firstStorage.insert(_, genesis, false))
              _             <- firstStorage.close()
              secondStorage <- createAtDefaultLocation(dagDataDir)
              result        <- lookupElements(blockElements, secondStorage)
              _             <- secondStorage.close()
            } yield testLookupElementsResult(
              result,
              blockElements ++ secondBlockElements ++ thirdBlockElements
            )
          }
      }
    }
  }

  it should "be able to restore equivocations tracker on startup" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      forAll(validatorGen) { equivocator =>
        forAll(blockHashGen) { blockHash =>
          withDagStorageLocation { dagDataDir =>
            for {
              firstStorage <- createAtDefaultLocation(dagDataDir)
              _            <- blockElements.traverse_(firstStorage.insert(_, genesis, false))
              record = EquivocationRecord(
                equivocator,
                0,
                Set(blockHash)
              )
              _ <- firstStorage.accessEquivocationsTracker { tracker =>
                    tracker.insertEquivocationRecord(record)
                  }
              _             <- firstStorage.close()
              secondStorage <- createAtDefaultLocation(dagDataDir)
              records       <- secondStorage.accessEquivocationsTracker(_.equivocationRecords)
              _             = records shouldBe Set(record)
              result        <- lookupElements(blockElements, secondStorage)
              _             <- secondStorage.close()
            } yield testLookupElementsResult(result, blockElements)
          }
        }
      }
    }
  }

  it should "be able to modify equivocation records" in {
    forAll(validatorGen, blockHashGen, blockHashGen) { (equivocator, blockHash1, blockHash2) =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage <- createAtDefaultLocation(dagDataDir)
          record       = EquivocationRecord(equivocator, 0, Set(blockHash1))
          _ <- firstStorage.accessEquivocationsTracker { tracker =>
                tracker.insertEquivocationRecord(record)
              }
          _ <- firstStorage.accessEquivocationsTracker { tracker =>
                tracker.updateEquivocationRecord(record, blockHash2)
              }
          _             <- firstStorage.close()
          updatedRecord = EquivocationRecord(equivocator, 0, Set(blockHash1, blockHash2))
          secondStorage <- createAtDefaultLocation(dagDataDir)
          records       <- secondStorage.accessEquivocationsTracker(_.equivocationRecords)
          _             = records shouldBe Set(updatedRecord)
          _             <- secondStorage.close()
        } yield ()
      }
    }
  }

  it should "be able to restore equivocations tracker on startup with appended garbage equivocation record" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      forAll(validatorGen) { equivocator =>
        forAll(blockHashGen) { blockHash =>
          withDagStorageLocation { dagDataDir =>
            for {
              firstStorage <- createAtDefaultLocation(dagDataDir)
              _            <- blockElements.traverse_(firstStorage.insert(_, genesis, false))
              record = EquivocationRecord(
                equivocator,
                0,
                Set(blockHash)
              )
              _ <- firstStorage.accessEquivocationsTracker { tracker =>
                    tracker.insertEquivocationRecord(record)
                  }
              _                  <- firstStorage.close()
              garbageEquivocator = Array.fill[Byte](Validator.Length)(0)
              _                  <- Sync[Task].delay { Random.nextBytes(garbageEquivocator) }
              garbageBlockHash   = Array.fill[Byte](BlockHash.Length)(0)
              _                  <- Sync[Task].delay { Random.nextBytes(garbageBlockHash) }
              garbageRecord = EquivocationRecord(
                ByteString.copyFrom(garbageEquivocator),
                0,
                Set(ByteString.copyFrom(garbageBlockHash))
              )
              garbageBytes = (codecValidator ~ int32 ~ codecBlockHashSet)
                .encode(
                  (
                    (garbageRecord.equivocator, garbageRecord.equivocationBaseBlockSeqNum),
                    garbageRecord.equivocationDetectedBlockHashes
                  )
                )
                .get
                .toByteArray
              _ <- writeToFile[Task](
                    defaultEquivocationsTrackerLog(dagDataDir),
                    garbageBytes,
                    StandardOpenOption.APPEND
                  )
              secondStorage <- createAtDefaultLocation(dagDataDir)
              records       <- secondStorage.accessEquivocationsTracker(_.equivocationRecords)
              _             = records shouldBe Set(record)
              result        <- lookupElements(blockElements, secondStorage)
              _             <- secondStorage.close()
            } yield testLookupElementsResult(result, blockElements)
          }
        }
      }
    }
  }

  it should "be able to restore invalid blocks on startup" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage  <- createAtDefaultLocation(dagDataDir)
          _             <- blockElements.traverse_(firstStorage.insert(_, genesis, true))
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dagDataDir)
          dag           <- secondStorage.getRepresentation
          invalidBlocks <- dag.invalidBlocks
          _             <- secondStorage.close()
        } yield invalidBlocks shouldBe blockElements.map(BlockMetadata.fromBlock(_, true)).toSet
      }
    }
  }

  it should "be able to restore deploy index on startup" in {
    forAll(blockElementsWithParentsGen, minSize(0), sizeRange(10)) { blockElements =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage  <- createAtDefaultLocation(dagDataDir)
          _             <- blockElements.traverse_(firstStorage.insert(_, genesis, true))
          _             <- firstStorage.close()
          secondStorage <- createAtDefaultLocation(dagDataDir)
          dag           <- secondStorage.getRepresentation
          (deploys, blockHashes) = blockElements
            .flatMap(b => b.body.deploys.map(_ -> b.blockHash))
            .unzip
          deployLookups <- deploys.traverse(d => dag.lookupByDeployId(d.deploy.sig))
          _             <- secondStorage.close()
        } yield deployLookups shouldBe blockHashes.map(_.some)
      }
    }
  }

  it should "be able to load checkpoints" in {
    forAll(blockElementsWithParentsGen, minSize(1), sizeRange(2)) { blockElements =>
      withDagStorageLocation { dagDataDir =>
        for {
          firstStorage <- createAtDefaultLocation(dagDataDir)
          _ <- blockElements.traverse_(
                b => firstStorage.insert(b, genesis, false)
              )
          _ <- firstStorage.close()
          _ <- Sync[Task].delay {
                Files.move(
                  defaultBlockMetadataLog(dagDataDir),
                  defaultCheckpointsDir(dagDataDir).resolve("0-1")
                )
                Files.delete(defaultBlockMetadataCrc(dagDataDir))
              }
          secondStorage <- createAtDefaultLocation(dagDataDir)
          result        <- lookupElements(blockElements, secondStorage)
          _             <- secondStorage.close()
        } yield testLookupElementsResult(
          result,
          blockElements
        )
      }
    }
  }
}
