package coop.rchain.blockstorage

import java.nio.{BufferUnderflowException, ByteBuffer}
import java.nio.file.{Path, StandardCopyOption}

import cats.Monad
import cats.implicits._
import cats.effect.{Concurrent, Resource, Sync}
import cats.effect.concurrent.Semaphore
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagFileStorage.{Checkpoint, CheckpointedDagInfo}
import coop.rchain.blockstorage.util.BlockMessageUtil.{blockNumber, bonds, parentHashes}
import coop.rchain.blockstorage.util.{BlockMessageUtil, Crc32, TopologicalSortUtil}
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Validator
import coop.rchain.models.{BlockMetadata, EquivocationRecord}
import coop.rchain.shared.{AtomicMonadState, Log, LogSource}
import coop.rchain.shared.ByteStringOps._
import coop.rchain.shared.Language.ignore
import monix.execution.atomic.AtomicAny
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Env, EnvFlags}

import scala.ref.WeakReference
import scala.util.matching.Regex

private final case class BlockDagFileStorageState[F[_]: Sync](
    latestMessages: Map[Validator, BlockHash],
    childMap: Map[BlockHash, Set[BlockHash]],
    dataLookup: Map[BlockHash, BlockMetadata],
    topoSort: Vector[Vector[BlockHash]],
    equivocationsTracker: Set[EquivocationRecord],
    invalidBlocks: Set[BlockMetadata],
    sortOffset: Long,
    checkpoints: List[Checkpoint],
    latestMessagesLogOutputStream: FileOutputStreamIO[F],
    latestMessagesLogSize: Int,
    latestMessagesCrc: Crc32[F],
    blockMetadataLogOutputStream: FileOutputStreamIO[F],
    blockMetadataCrc: Crc32[F],
    equivocationsTrackerLogOutputStream: FileOutputStreamIO[F],
    equivocationsTrackerCrc: Crc32[F],
    invalidBlocksLogOutputStream: FileOutputStreamIO[F],
    invalidBlocksCrc: Crc32[F]
)

final class BlockDagFileStorage[F[_]: Concurrent: Sync: Log: RaiseIOError] private (
    lock: Semaphore[F],
    blockNumberIndex: LmdbDbi[F, ByteBuffer],
    latestMessagesDataFilePath: Path,
    latestMessagesCrcFilePath: Path,
    latestMessagesLogMaxSizeFactor: Int,
    blockMetadataLogPath: Path,
    blockMetadataCrcPath: Path,
    equivocationTrackerLogPath: Path,
    equivocationTrackerCrcPath: Path,
    invalidBlocksLogPath: Path,
    invalidBlocksCrcPath: Path,
    state: MonadState[F, BlockDagFileStorageState[F]]
) extends BlockDagStorage[F] {
  implicit private val logSource = LogSource(BlockDagFileStorage.getClass)

  private[this] def getLatestMessages: F[Map[Validator, BlockHash]] =
    state.get.map(_.latestMessages)
  private[this] def getChildMap: F[Map[BlockHash, Set[BlockHash]]] =
    state.get.map(_.childMap)
  private[this] def getDataLookup: F[Map[BlockHash, BlockMetadata]] =
    state.get.map(_.dataLookup)
  private[this] def getTopoSort: F[Vector[Vector[BlockHash]]] =
    state.get.map(_.topoSort)
  private[this] def getSortOffset: F[Long] =
    state.get.map(_.sortOffset)
  private[this] def getEquviocationsTracker: F[Set[EquivocationRecord]] =
    state.get.map(_.equivocationsTracker)
  private[this] def getInvalidBlocks: F[Set[BlockMetadata]] =
    state.get.map(_.invalidBlocks)
  private[this] def getCheckpoints: F[List[Checkpoint]] =
    state.get.map(_.checkpoints)
  private[this] def getLatestMessagesLogOutputStream: F[FileOutputStreamIO[F]] =
    state.get.map(_.latestMessagesLogOutputStream)
  private[this] def getLatestMessagesLogSize: F[Int] =
    state.get.map(_.latestMessagesLogSize)
  private[this] def getLatestMessagesCrc: F[Crc32[F]] =
    state.get.map(_.latestMessagesCrc)
  private[this] def getBlockMetadataLogOutputStream: F[FileOutputStreamIO[F]] =
    state.get.map(_.blockMetadataLogOutputStream)
  private[this] def getBlockMetadataCrc: F[Crc32[F]] =
    state.get.map(_.blockMetadataCrc)
  private[this] def getEquivocationsTrackerLogOutputStream: F[FileOutputStreamIO[F]] =
    state.get.map(_.equivocationsTrackerLogOutputStream)
  private[this] def getEquivocationsTrackerCrc: F[Crc32[F]] =
    state.get.map(_.equivocationsTrackerCrc)
  private[this] def getInvalidBlocksLogOutputStream: F[FileOutputStreamIO[F]] =
    state.get.map(_.invalidBlocksLogOutputStream)
  private[this] def getInvalidBlocksCrc: F[Crc32[F]] =
    state.get.map(_.invalidBlocksCrc)

  private[this] def setLatestMessages(v: Map[Validator, BlockHash]): F[Unit] =
    state.modify(s => s.copy(latestMessages = v))
  private[this] def setChildMap(v: Map[BlockHash, Set[BlockHash]]): F[Unit] =
    state.modify(s => s.copy(childMap = v))
  private[this] def setDataLookup(v: Map[BlockHash, BlockMetadata]): F[Unit] =
    state.modify(s => s.copy(dataLookup = v))
  private[this] def setTopoSort(v: Vector[Vector[BlockHash]]): F[Unit] =
    state.modify(s => s.copy(topoSort = v))
  private[this] def setEquviocationsTracker(v: Set[EquivocationRecord]): F[Unit] =
    state.modify(s => s.copy(equivocationsTracker = v))
  private[this] def setLatestMessagesLogOutputStream(v: FileOutputStreamIO[F]): F[Unit] =
    state.modify(s => s.copy(latestMessagesLogOutputStream = v))
  private[this] def setLatestMessagesLogSize(v: Int): F[Unit] =
    state.modify(s => s.copy(latestMessagesLogSize = v))
  private[this] def setLatestMessagesCrc(v: Crc32[F]): F[Unit] =
    state.modify(s => s.copy(latestMessagesCrc = v))
  private[this] def setBlockMetadataLogOutputStream(v: FileOutputStreamIO[F]): F[Unit] =
    state.modify(s => s.copy(blockMetadataLogOutputStream = v))
  private[this] def setBlockMetadataCrc(v: Crc32[F]): F[Unit] =
    state.modify(s => s.copy(blockMetadataCrc = v))
  private[this] def setEquivocationsTrackerLogOutputStream(v: FileOutputStreamIO[F]): F[Unit] =
    state.modify(s => s.copy(equivocationsTrackerLogOutputStream = v))

  private[this] def modifyLatestMessages(
      f: Map[Validator, BlockHash] => Map[Validator, BlockHash]
  ): F[Unit] =
    state.modify(s => s.copy(latestMessages = f(s.latestMessages)))
  private[this] def modifyChildMap(
      f: Map[BlockHash, Set[BlockHash]] => Map[BlockHash, Set[BlockHash]]
  ): F[Unit] =
    state.modify(s => s.copy(childMap = f(s.childMap)))
  private[this] def modifyDataLookup(
      f: Map[BlockHash, BlockMetadata] => Map[BlockHash, BlockMetadata]
  ): F[Unit] =
    state.modify(s => s.copy(dataLookup = f(s.dataLookup)))
  private[this] def modifyTopoSort(
      f: Vector[Vector[BlockHash]] => Vector[Vector[BlockHash]]
  ): F[Unit] =
    state.modify(s => s.copy(topoSort = f(s.topoSort)))
  private[this] def modifyEquivocationsTracker(
      f: Set[EquivocationRecord] => Set[EquivocationRecord]
  ): F[Unit] =
    state.modify(s => s.copy(equivocationsTracker = f(s.equivocationsTracker)))
  private[this] def modifyInvalidBlocks(
      f: Set[BlockMetadata] => Set[BlockMetadata]
  ): F[Unit] =
    state.modify(s => s.copy(invalidBlocks = f(s.invalidBlocks)))
  private[this] def modifyCheckpoints(f: List[Checkpoint] => List[Checkpoint]): F[Unit] =
    state.modify(s => s.copy(checkpoints = f(s.checkpoints)))
  private[this] def modifyLatestMessagesLogSize(f: Int => Int): F[Unit] =
    state.modify(s => s.copy(latestMessagesLogSize = f(s.latestMessagesLogSize)))

  private[this] def getBlockNumber(blockHash: BlockHash): F[Option[Long]] =
    blockNumberIndex.withReadTxn { txn =>
      blockNumberIndex
        .get(txn, blockHash.toDirectByteBuffer)
        .map(_.getLong)
    }

  private[this] def putBlockNumber(blockHash: BlockHash, blockNumber: Long): F[Unit] =
    blockNumberIndex.withWriteTxn { txn =>
      ignore {
        blockNumberIndex.put(
          txn,
          blockHash.toDirectByteBuffer,
          blockNumber.toByteString.toDirectByteBuffer
        )
      }
    }

  private case class FileDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]],
      invalidBlocksSet: Set[BlockMetadata],
      sortOffset: Long
  ) extends BlockDagRepresentation[F] {
    private def findAndAccessCheckpoint[R](
        blockHash: BlockHash,
        loadFromCheckpoint: CheckpointedDagInfo => Option[R]
    ): F[Option[R]] =
      for {
        blockNumberOpt <- getBlockNumber(blockHash)
        result <- blockNumberOpt match {
                   case Some(blockNumber) =>
                     if (blockNumber >= sortOffset) {
                       none[R].pure[F]
                     } else {
                       lock.withPermit(
                         loadCheckpoint(blockNumber).map(_.flatMap(loadFromCheckpoint))
                       )
                     }
                   case None =>
                     none[R].pure[F]
                 }
      } yield result

    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      for {
        result <- childMap.get(blockHash) match {
                   case children: Some[Set[BlockHash]] =>
                     Monad[F].pure[Option[Set[BlockHash]]](children)
                   case None =>
                     findAndAccessCheckpoint(blockHash, _.childMap.get(blockHash))
                 }
      } yield result
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup.get(blockHash) match {
        case blockMetadata: Some[BlockMetadata] =>
          Monad[F].pure[Option[BlockMetadata]](blockMetadata)
        case None =>
          findAndAccessCheckpoint(blockHash, _.dataLookup.get(blockHash))
      }
    def contains(blockHash: BlockHash): F[Boolean] =
      if (blockHash.size == BlockHash.Length) {
        dataLookup.get(blockHash) match {
          case Some(_) => true.pure[F]
          case None    => getBlockNumber(blockHash).map(_.isDefined)
        }
      } else {
        false.pure[F]
      }
    def topoSort(startBlockNumber: Long): F[Vector[Vector[BlockHash]]] =
      if (startBlockNumber >= sortOffset) {
        val offset = startBlockNumber - sortOffset
        assert(offset.isValidInt)
        topoSortVector.drop(offset.toInt).pure[F]
      } else if (sortOffset - startBlockNumber + topoSortVector.length < Int.MaxValue) { // Max Vector length
        lock.withPermit(
          for {
            checkpoints          <- getCheckpoints
            checkpointsWithIndex = checkpoints.zipWithIndex
            checkpointsToLoad    = checkpointsWithIndex.filter(startBlockNumber < _._1.end)
            checkpointsDagInfos <- checkpointsToLoad.traverse {
                                    case (startingCheckpoint, index) =>
                                      loadCheckpointDagInfo(startingCheckpoint, index)
                                  }
            topoSortPrefix = checkpointsDagInfos.toVector.flatMap { checkpointsDagInfo =>
              val offset = startBlockNumber - checkpointsDagInfo.sortOffset
              // offset is always a valid Int since the method result's length was validated before
              checkpointsDagInfo.topoSort.drop(offset.toInt) // negative drops are ignored
            }
            result = topoSortPrefix ++ topoSortVector
          } yield result
        )
      } else {
        Sync[F].raiseError(
          TopoSortLengthIsTooBig(sortOffset - startBlockNumber + topoSortVector.length)
        )
      }
    // TODO should startBlockNumber have topoSortVector.length - 1 (off by one error)?
    def topoSortTail(tailLength: Int): F[Vector[Vector[BlockHash]]] = {
      val startBlockNumber = Math.max(0L, sortOffset - (tailLength - topoSortVector.length))
      topoSort(startBlockNumber)
    }
    def deriveOrdering(startBlockNumber: Long): F[Ordering[BlockMetadata]] =
      topoSort(startBlockNumber).map { topologicalSorting =>
        val order = topologicalSorting.flatten.zipWithIndex.toMap
        Ordering.by(b => order(b.blockHash))
      }
    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      latestMessagesMap.get(validator).pure[F]
    def latestMessage(validator: Validator): F[Option[BlockMetadata]] =
      latestMessagesMap.get(validator).flatTraverse(lookup)
    def latestMessageHashes: F[Map[Validator, BlockHash]] =
      latestMessagesMap.pure[F]
    def latestMessages: F[Map[Validator, BlockMetadata]] =
      latestMessagesMap.toList
        .traverse {
          case (validator, hash) => lookup(hash).map(validator -> _.get)
        }
        .map(_.toMap)
    def invalidBlocks: F[Set[BlockMetadata]] =
      invalidBlocksSet.pure[F]
  }

  private object FileEquivocationsTracker extends EquivocationsTracker[F] {
    override def equivocationRecords: F[Set[EquivocationRecord]] =
      getEquviocationsTracker
    override def insertEquivocationRecord(record: EquivocationRecord): F[Unit] =
      modifyEquivocationsTracker(_ + record) >>
        updateEquivocationsTrackerFile(record)
    override def updateEquivocationRecord(
        record: EquivocationRecord,
        blockHash: BlockHash
    ): F[Unit] = {
      val updatedEquivocationDetectedBlockHashes =
        record.equivocationDetectedBlockHashes + blockHash
      val newRecord =
        record.copy(equivocationDetectedBlockHashes = updatedEquivocationDetectedBlockHashes)
      modifyEquivocationsTracker(_ - record + newRecord) >>
        updateEquivocationsTrackerFile(newRecord)
    }
  }

  private def loadDagInfo(checkpoint: Checkpoint): F[CheckpointedDagInfo] = {
    val checkpointDataInputResource = Resource.make(
      RandomAccessIO.open[F](checkpoint.path, RandomAccessIO.Read)
    )(_.close)
    for {
      blockMetadataList <- checkpointDataInputResource.use { checkpointDataInput =>
                            BlockDagFileStorage.readDataLookupData(checkpointDataInput)
                          }
      dataLookup = blockMetadataList.toMap
      childMap   = BlockDagFileStorage.extractChildMap(blockMetadataList)
      topoSort   = BlockDagFileStorage.extractTopoSort(blockMetadataList)
    } yield CheckpointedDagInfo(childMap, dataLookup, topoSort, checkpoint.start)
  }

  private def loadCheckpointDagInfo(checkpoint: Checkpoint, index: Int): F[CheckpointedDagInfo] =
    checkpoint.dagInfo.flatMap(_.get) match {
      case Some(dagInfo) =>
        dagInfo.pure[F]
      case None =>
        for {
          loadedDagInfo <- loadDagInfo(checkpoint)
          newCheckpoint = checkpoint.copy(dagInfo = Some(WeakReference(loadedDagInfo)))
          _             <- modifyCheckpoints(_.patch(index, List(newCheckpoint), 1))
        } yield loadedDagInfo
    }

  private def loadCheckpoint(offset: Long): F[Option[CheckpointedDagInfo]] =
    for {
      checkpoints <- getCheckpoints
      neededCheckpoint = checkpoints.zipWithIndex.find {
        case (c, _) => c.start <= offset && offset < c.end
      }
      result <- neededCheckpoint match {
                 case None =>
                   Log[F].warn(
                     s"Requested a block with block number $offset, but there is no checkpoint for it"
                   ) *> None.pure[F]
                 case Some((checkpoint, i)) =>
                   loadCheckpointDagInfo(checkpoint, i).map(Option(_))
               }
    } yield result

  private def updateCrcFile(newCrc: Crc32[F], crcFilePath: Path): F[Unit] =
    for {
      newCrcBytes <- newCrc.bytes
      tmpCrc      <- createSameDirectoryTemporaryFile(crcFilePath)
      _           <- writeToFile[F](tmpCrc, newCrcBytes)
      _           <- replaceFile(tmpCrc, crcFilePath)
    } yield ()

  private def updateLatestMessagesFile(newLatestMessages: List[(Validator, BlockHash)]): F[Unit] =
    for {
      latestMessagesCrc <- getLatestMessagesCrc
      _ <- newLatestMessages.traverse_ {
            case (validator, blockHash) =>
              val toAppend = validator.concat(blockHash).toByteArray
              for {
                latestMessagesLogOutputStream <- getLatestMessagesLogOutputStream
                _                             <- latestMessagesLogOutputStream.write(toAppend)
                _                             <- latestMessagesLogOutputStream.flush
                _ <- latestMessagesCrc.update(toAppend).flatMap { _ =>
                      updateCrcFile(latestMessagesCrc, latestMessagesCrcFilePath)
                    }
              } yield ()
          }
      _ <- modifyLatestMessagesLogSize(_ + 1)
    } yield ()

  private def replaceFile(from: Path, to: Path): F[Path] =
    moveFile[F](from, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  private def squashLatestMessagesDataFile(): F[Unit] =
    for {
      latestMessages                <- getLatestMessages
      latestMessagesLogOutputStream <- getLatestMessagesLogOutputStream
      _                             <- latestMessagesLogOutputStream.close
      tmpSquashedData               <- createSameDirectoryTemporaryFile(latestMessagesDataFilePath)
      tmpSquashedCrc                <- createSameDirectoryTemporaryFile(latestMessagesCrcFilePath)
      dataByteBuffer = ByteBuffer.allocate(
        (Validator.Length + BlockHash.Length) * latestMessages.size
      )
      _ <- latestMessages.toList.traverse_ {
            case (validator, blockHash) =>
              Sync[F].delay {
                ignore { dataByteBuffer.put(validator.toByteArray) }
                ignore { dataByteBuffer.put(blockHash.toByteArray) }
              }
          }
      _                <- writeToFile[F](tmpSquashedData, dataByteBuffer.array())
      squashedCrc      = Crc32.empty[F]()
      _                <- squashedCrc.update(dataByteBuffer.array())
      squashedCrcBytes <- squashedCrc.bytes
      _                <- writeToFile[F](tmpSquashedCrc, squashedCrcBytes)
      _                <- replaceFile(tmpSquashedData, latestMessagesDataFilePath)
      _                <- replaceFile(tmpSquashedCrc, latestMessagesCrcFilePath)
      newLatestMessagesLogOutputStream <- FileOutputStreamIO
                                           .open[F](latestMessagesDataFilePath, true)
      _ <- setLatestMessagesLogOutputStream(newLatestMessagesLogOutputStream)
      _ <- setLatestMessagesCrc(squashedCrc)
      _ <- setLatestMessagesLogSize(0)
    } yield ()

  private def squashLatestMessagesDataFileIfNeeded(): F[Unit] =
    for {
      latestMessages        <- getLatestMessages
      latestMessagesLogSize <- getLatestMessagesLogSize
      result <- if (latestMessagesLogSize > latestMessages.size * latestMessagesLogMaxSizeFactor) {
                 squashLatestMessagesDataFile()
               } else {
                 ().pure[F]
               }
    } yield result

  private def updateDataLookupFile(blockMetadata: BlockMetadata): F[Unit] =
    for {
      dataLookupCrc          <- getBlockMetadataCrc
      blockBytes             = blockMetadata.toByteString
      toAppend               = blockBytes.size.toByteString.concat(blockBytes).toByteArray
      dataLookupOutputStream <- getBlockMetadataLogOutputStream
      _                      <- dataLookupOutputStream.write(toAppend)
      _                      <- dataLookupOutputStream.flush
      _                      <- dataLookupCrc.update(toAppend)
      _                      <- updateCrcFile(dataLookupCrc, blockMetadataCrcPath)
    } yield ()

  private def updateEquivocationsTrackerFile(equivocationRecord: EquivocationRecord): F[Unit] =
    for {
      equivocationsTrackerCrc             <- getEquivocationsTrackerCrc
      equivocationsTrackerLogOutputStream <- getEquivocationsTrackerLogOutputStream
      toAppend                            = equivocationRecord.toByteString.toByteArray
      _                                   <- equivocationsTrackerLogOutputStream.write(toAppend)
      _                                   <- equivocationsTrackerLogOutputStream.flush
      _                                   <- equivocationsTrackerCrc.update(toAppend)
      _                                   <- updateCrcFile(equivocationsTrackerCrc, equivocationTrackerCrcPath)
    } yield ()

  private def updateInvalidBlocksFile(newBlockMetadata: BlockMetadata): F[Unit] =
    for {
      invalidBlocksCrc             <- getInvalidBlocksCrc
      invalidBlocksLogOutputStream <- getInvalidBlocksLogOutputStream
      blockBytes                   = newBlockMetadata.toByteString
      toAppend                     = blockBytes.size.toByteString.concat(blockBytes).toByteArray
      _                            <- invalidBlocksLogOutputStream.write(toAppend)
      _                            <- invalidBlocksLogOutputStream.flush
      _                            <- invalidBlocksCrc.update(toAppend)
      _                            <- updateCrcFile(invalidBlocksCrc, invalidBlocksCrcPath)
    } yield ()

  private def representation: F[BlockDagRepresentation[F]] =
    for {
      latestMessages <- getLatestMessages
      childMap       <- getChildMap
      dataLookup     <- getDataLookup
      topoSort       <- getTopoSort
      invalidBlocks  <- getInvalidBlocks
      sortOffset     <- getSortOffset
    } yield FileDagRepresentation(
      latestMessages,
      childMap,
      dataLookup,
      topoSort,
      invalidBlocks,
      sortOffset
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    lock.withPermit(
      for {
        alreadyStored <- getDataLookup.map(_.contains(block.blockHash))
        _ <- if (alreadyStored) {
              Log[F].warn(s"Block ${Base16.encode(block.blockHash.toByteArray)} is already stored")
            } else {
              for {
                _             <- squashLatestMessagesDataFileIfNeeded()
                blockMetadata = BlockMetadata.fromBlock(block, invalid)
                _             = assert(block.blockHash.size == BlockHash.Length)
                _             <- if (invalid) modifyInvalidBlocks(_ + blockMetadata) else ().pure[F]
                _             <- modifyDataLookup(_.updated(block.blockHash, blockMetadata))
                _ <- modifyChildMap(
                      childMap =>
                        parentHashes(block)
                          .foldLeft(childMap) {
                            case (acc, p) =>
                              val currChildren = acc.getOrElse(p, Set.empty[BlockHash])
                              acc.updated(p, currChildren + block.blockHash)
                          }
                          .updated(block.blockHash, Set.empty[BlockHash])
                    )
                _ <- modifyTopoSort(topoSort => TopologicalSortUtil.update(topoSort, 0L, block))
                //Block which contains newly bonded validators will not
                //have those validators in its justification
                newValidators = bonds(block)
                  .map(_.validator)
                  .toSet
                  .diff(block.justifications.map(_.validator).toSet)
                newValidatorsLatestMessages = newValidators.map(v => (v, genesis.blockHash))
                newValidatorsWithSenderLatestMessages <- if (block.sender.isEmpty) {
                                                          // Ignore empty sender for special cases such as genesis block
                                                          Log[F].warn(
                                                            s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                          ) *> newValidatorsLatestMessages.pure[F]
                                                        } else if (block.sender
                                                                     .size() == Validator.Length) {
                                                          (newValidatorsLatestMessages + (
                                                            (
                                                              block.sender,
                                                              block.blockHash
                                                            )
                                                          )).pure[F]
                                                        } else {
                                                          Sync[F].raiseError[Set[
                                                            (ByteString, ByteString)
                                                          ]](
                                                            BlockSenderIsMalformed(block)
                                                          )
                                                        }
                _ <- modifyLatestMessages { latestMessages =>
                      newValidatorsWithSenderLatestMessages.foldLeft(latestMessages) {
                        //Update new validators with block in which
                        //they were bonded (i.e. this block for the sender and genesis for newly bonded validators)
                        case (acc, (validator, blockHash)) => acc.updated(validator, blockHash)
                      }
                    }
                _ <- putBlockNumber(block.blockHash, blockNumber(block))
                _ <- updateLatestMessagesFile(newValidatorsWithSenderLatestMessages.toList)
                _ <- updateDataLookupFile(blockMetadata)
                _ <- updateInvalidBlocksFile(blockMetadata)
              } yield ()
            }
        dag <- representation
      } yield dag
    )

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(
      f(FileEquivocationsTracker)
    )

  def checkpoint(): F[Unit] =
    ().pure[F]

  def clear(): F[Unit] =
    lock.withPermit(
      for {
        latestMessagesLogOutputStream <- getLatestMessagesLogOutputStream
        _                             <- latestMessagesLogOutputStream.close
        blockMetadataLogOutputStream  <- getBlockMetadataLogOutputStream
        _                             <- blockMetadataLogOutputStream.close
        _                             <- writeToFile(latestMessagesDataFilePath, Array.emptyByteArray)
        _                             <- writeToFile(blockMetadataLogPath, Array.emptyByteArray)
        newLatestMessagesCrc          = Crc32.empty[F]()
        newLatestMessagesCrcBytes     <- newLatestMessagesCrc.bytes
        _                             <- writeToFile(latestMessagesCrcFilePath, newLatestMessagesCrcBytes)
        newBlockMetadataCrc           = Crc32.empty[F]()
        newBlockMetadataCrcBytes      <- newBlockMetadataCrc.bytes
        _                             <- writeToFile(blockMetadataCrcPath, newBlockMetadataCrcBytes)
        _                             <- setDataLookup(Map.empty)
        _                             <- setChildMap(Map.empty)
        _                             <- setTopoSort(Vector.empty)
        _                             <- setLatestMessages(Map.empty)
        _                             <- setEquviocationsTracker(Set.empty)
        newLatestMessagesLogOutputStream <- FileOutputStreamIO
                                             .open[F](latestMessagesDataFilePath, true)
        _                               <- setLatestMessagesLogOutputStream(newLatestMessagesLogOutputStream)
        newBlockMetadataLogOutputStream <- FileOutputStreamIO.open[F](blockMetadataLogPath, true)
        _                               <- setBlockMetadataLogOutputStream(newBlockMetadataLogOutputStream)
        newEquviocationsTrackerLogOutputStream <- FileOutputStreamIO
                                                   .open[F](equivocationTrackerLogPath, true)
        _ <- setEquivocationsTrackerLogOutputStream(newEquviocationsTrackerLogOutputStream)
        _ <- setLatestMessagesLogSize(0)
        _ <- setLatestMessagesCrc(newLatestMessagesCrc)
        _ <- setBlockMetadataCrc(newBlockMetadataCrc)
        _ <- blockNumberIndex.withWriteTxn { txn =>
              blockNumberIndex.drop(txn)
            }
      } yield ()
    )

  def close(): F[Unit] =
    lock.withPermit(
      for {
        latestMessagesLogOutputStream       <- getLatestMessagesLogOutputStream
        _                                   <- latestMessagesLogOutputStream.close
        blockMetadataLogOutputStream        <- getBlockMetadataLogOutputStream
        _                                   <- blockMetadataLogOutputStream.close
        equivocationsTrackerLogOutputStream <- getEquivocationsTrackerLogOutputStream
        _                                   <- equivocationsTrackerLogOutputStream.close
        _                                   <- blockNumberIndex.close
      } yield ()
    )
}

object BlockDagFileStorage {
  val IntSize = 4L

  implicit private val logSource       = LogSource(BlockDagFileStorage.getClass)
  private val checkpointPattern: Regex = "([0-9]+)-([0-9]+)".r

  final case class Config(
      latestMessagesLogPath: Path,
      latestMessagesCrcPath: Path,
      blockMetadataLogPath: Path,
      blockMetadataCrcPath: Path,
      equivocationsTrackerLogPath: Path,
      equivocationsTrackerCrcPath: Path,
      invalidBlocksLogPath: Path,
      invalidBlocksCrcPath: Path,
      checkpointsDirPath: Path,
      blockNumberIndexPath: Path,
      mapSize: Long,
      latestMessagesLogMaxSizeFactor: Int = 10,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  private[blockstorage] final case class CheckpointedDagInfo(
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSort: Vector[Vector[BlockHash]],
      sortOffset: Long
  )

  private[blockstorage] final case class Checkpoint(
      start: Long,
      end: Long,
      path: Path,
      dagInfo: Option[WeakReference[CheckpointedDagInfo]]
  )

  private def readCrc[F[_]: Sync: Log: RaiseIOError](crcPath: Path): F[Long] =
    for {
      _          <- createNewFile[F](crcPath)
      bytes      <- readAllBytesFromFile[F](crcPath)
      byteBuffer = ByteBuffer.wrap(bytes)
      result <- Sync[F].delay { byteBuffer.getLong() }.handleErrorWith {
                 case _: BufferUnderflowException =>
                   for {
                     _ <- Log[F].warn(s"CRC file $crcPath did not contain a valid CRC value")
                   } yield 0
                 case exception =>
                   Sync[F].raiseError(exception)
               }
    } yield result

  private def calculateLatestMessagesCrc[F[_]: Monad](
      latestMessagesList: List[(Validator, BlockHash)]
  ): Crc32[F] =
    Crc32[F](
      latestMessagesList
        .foldLeft(ByteString.EMPTY) {
          case (byteString, (validator, blockHash)) =>
            byteString.concat(validator.concat(blockHash))
        }
        .toByteArray
    )

  private def readLatestMessagesData[F[_]: Sync: Log](
      randomAccessIO: RandomAccessIO[F]
  ): F[(List[(Validator, BlockHash)], Int)] = {
    def readRec(
        result: List[(Validator, BlockHash)],
        logSize: Int
    ): F[(List[(Validator, BlockHash)], Int)] = {
      val validatorPk = Array.fill[Byte](Validator.Length)(0)
      val blockHash   = Array.fill[Byte](BlockHash.Length)(0)
      for {
        validatorPkRead <- randomAccessIO.readFully(validatorPk)
        blockHashRead   <- randomAccessIO.readFully(blockHash)
        result <- (validatorPkRead, blockHashRead) match {
                   case (Some(_), Some(_)) =>
                     val pair = (ByteString.copyFrom(validatorPk), ByteString.copyFrom(blockHash))
                     readRec(
                       pair :: result,
                       logSize + 1
                     )
                   case (None, None) =>
                     (result.reverse, logSize).pure[F]
                   case (_, _) =>
                     for {
                       _ <- Log[F].error("Latest messages log is malformed")
                       result <- Sync[F].raiseError[(List[(Validator, BlockHash)], Int)](
                                  LatestMessagesLogIsMalformed
                                )
                     } yield result
                 }
      } yield result
    }
    readRec(List.empty, 0)
  }

  private def validateLatestMessagesData[F[_]: Sync](
      latestMessagesRaf: RandomAccessIO[F],
      readLatestMessagesCrc: Long,
      latestMessagesCrcPath: Path,
      latestMessagesList: List[(Validator, BlockHash)]
  ): F[(Map[Validator, BlockHash], Crc32[F])] = {
    val fullCalculatedCrc = calculateLatestMessagesCrc[F](latestMessagesList)
    fullCalculatedCrc.value.flatMap { fullCalculatedCrcValue =>
      if (fullCalculatedCrcValue == readLatestMessagesCrc) {
        (latestMessagesList.toMap, fullCalculatedCrc).pure[F]
      } else {
        val withoutLastCalculatedCrc = calculateLatestMessagesCrc[F](latestMessagesList.init)
        withoutLastCalculatedCrc.value.flatMap { withoutLastCalculatedCrcValue =>
          if (withoutLastCalculatedCrcValue == readLatestMessagesCrc) {
            for {
              length <- latestMessagesRaf.length
              _      <- latestMessagesRaf.setLength(length - (Validator.Length + BlockHash.Length))
            } yield (latestMessagesList.init.toMap, withoutLastCalculatedCrc)
          } else {
            Sync[F].raiseError[(Map[Validator, BlockHash], Crc32[F])](LatestMessagesLogIsCorrupted)
          }
        }
      }
    }
  }

  private def calculateDataLookupCrc[F[_]: Monad](
      dataLookupList: List[(BlockHash, BlockMetadata)]
  ): Crc32[F] =
    Crc32[F](
      dataLookupList
        .foldLeft(ByteString.EMPTY) {
          case (byteString, (_, blockMetadata)) =>
            val blockBytes = blockMetadata.toByteString
            byteString.concat(blockBytes.size().toByteString.concat(blockBytes))
        }
        .toByteArray
    )

  private def readDataLookupData[F[_]: Sync](
      randomAccessIO: RandomAccessIO[F]
  ): F[List[(BlockHash, BlockMetadata)]] = {
    def readRec(
        result: List[(BlockHash, BlockMetadata)]
    ): F[List[(BlockHash, BlockMetadata)]] =
      for {
        blockSizeOpt <- randomAccessIO.readInt
        result <- blockSizeOpt match {
                   case Some(blockSize) =>
                     val blockMetaBytes = Array.ofDim[Byte](blockSize)
                     for {
                       _             <- randomAccessIO.readFully(blockMetaBytes)
                       blockMetadata <- Sync[F].delay { BlockMetadata.fromBytes(blockMetaBytes) }
                       result        <- readRec((blockMetadata.blockHash -> blockMetadata) :: result)
                     } yield result
                   case None =>
                     result.reverse.pure[F]
                 }
      } yield result
    readRec(List.empty)
  }

  private def validateDataLookupData[F[_]: Sync](
      dataLookupRandomAccessFile: RandomAccessIO[F],
      readDataLookupCrc: Long,
      dataLookupCrcPath: Path,
      dataLookupList: List[(BlockHash, BlockMetadata)]
  ): F[(List[(BlockHash, BlockMetadata)], Crc32[F])] = {
    val fullCalculatedCrc = calculateDataLookupCrc[F](dataLookupList)
    fullCalculatedCrc.value.flatMap { fullCalculatedCrcValue =>
      if (fullCalculatedCrcValue == readDataLookupCrc) {
        (dataLookupList, fullCalculatedCrc).pure[F]
      } else if (dataLookupList.nonEmpty) {
        val withoutLastCalculatedCrc = calculateDataLookupCrc[F](dataLookupList.init)
        withoutLastCalculatedCrc.value.flatMap { withoutLastCalculatedCrcValue =>
          if (withoutLastCalculatedCrcValue == readDataLookupCrc) {
            val byteString                    = dataLookupList.last._2.toByteString
            val lastDataLookupEntrySize: Long = IntSize + byteString.size()
            for {
              length <- dataLookupRandomAccessFile.length
              _      <- dataLookupRandomAccessFile.setLength(length - lastDataLookupEntrySize)
            } yield (dataLookupList.init, withoutLastCalculatedCrc)
          } else {
            Sync[F].raiseError[(List[(BlockHash, BlockMetadata)], Crc32[F])](DataLookupIsCorrupted)
          }
        }
      } else {
        Sync[F].raiseError[(List[(BlockHash, BlockMetadata)], Crc32[F])](DataLookupIsCorrupted)
      }
    }
  }

  private def calculateEquivocationsTrackerCrc[F[_]: Monad](
      equivocations: List[EquivocationRecord]
  ): Crc32[F] =
    Crc32[F](
      equivocations
        .foldLeft(ByteString.EMPTY) {
          case (byteString, record) =>
            byteString.concat(record.toByteString)
        }
        .toByteArray
    )

  private def readEquivocationsTrackerLog[F[_]: Sync](
      randomAccessIO: RandomAccessIO[F]
  ): F[List[EquivocationRecord]] = {
    def readRec(
        accumulator: List[EquivocationRecord]
    ): F[List[EquivocationRecord]] = {
      val equivocatorBytes = Array.ofDim[Byte](Validator.Length)
      for {
        equivocatorRead     <- randomAccessIO.readFully(equivocatorBytes)
        sequenceNumberOpt   <- randomAccessIO.readInt
        blockHashSetSizeOpt <- randomAccessIO.readInt
        result <- (equivocatorRead, sequenceNumberOpt, blockHashSetSizeOpt) match {
                   case (Some(()), Some(sequenceNumber), Some(blockHashSetSize)) =>
                     for {
                       blockHashes <- (0 until blockHashSetSize).toList.traverse { _ =>
                                       val blockHashBytes = Array.ofDim[Byte](BlockHash.Length)
                                       randomAccessIO.readFully(blockHashBytes).flatMap {
                                         case Some(()) =>
                                           ByteString.copyFrom(blockHashBytes).pure[F]
                                         case None =>
                                           Sync[F].raiseError[ByteString](
                                             EquivocationsTrackerLogIsMalformed
                                           )
                                       }
                                     }
                       readRecord = EquivocationRecord(
                         ByteString.copyFrom(equivocatorBytes),
                         sequenceNumber,
                         blockHashes.toSet
                       )
                       result <- readRec(readRecord :: accumulator)
                     } yield result
                   case (None, None, None) =>
                     accumulator.reverse.pure[F]
                   case _ =>
                     Sync[F].raiseError[List[EquivocationRecord]](
                       EquivocationsTrackerLogIsMalformed
                     )
                 }
      } yield result
    }
    readRec(List.empty)
  }

  private def validateEquivocationsTrackerData[F[_]: Sync](
      equivocationsTrackerRandomAccessIo: RandomAccessIO[F],
      readEquivocationsTrackerCrc: Long,
      equivocationsTrackerCrcPath: Path,
      equivocationsTracker: List[EquivocationRecord]
  ): F[(List[EquivocationRecord], Crc32[F])] = {
    val fullCalculatedCrc = calculateEquivocationsTrackerCrc[F](equivocationsTracker)
    fullCalculatedCrc.value.flatMap { fullCalculatedCrcValue =>
      if (fullCalculatedCrcValue == readEquivocationsTrackerCrc) {
        (equivocationsTracker, fullCalculatedCrc).pure[F]
      } else if (equivocationsTracker.nonEmpty) {
        val withoutLastCalculatedCrc =
          calculateEquivocationsTrackerCrc[F](equivocationsTracker.init)
        withoutLastCalculatedCrc.value.flatMap { withoutLastCalculatedCrcValue =>
          if (withoutLastCalculatedCrcValue == readEquivocationsTrackerCrc) {
            val lastRecord           = equivocationsTracker.last
            val blockHashesSize      = lastRecord.equivocationDetectedBlockHashes.size * BlockHash.Length
            val lastRecordSize: Long = Validator.Length + IntSize + IntSize + blockHashesSize
            for {
              length <- equivocationsTrackerRandomAccessIo.length
              _      <- equivocationsTrackerRandomAccessIo.setLength(length - lastRecordSize)
            } yield (equivocationsTracker.init, withoutLastCalculatedCrc)
          } else {
            Sync[F].raiseError[(List[EquivocationRecord], Crc32[F])](
              EquivocationsTrackerLogIsMalformed
            )
          }
        }
      } else {
        Sync[F].raiseError[(List[EquivocationRecord], Crc32[F])](
          EquivocationsTrackerLogIsMalformed
        )
      }
    }
  }

  private def squashEquivocationsTracker(
      equivocationsTrackerList: List[EquivocationRecord]
  ): Set[EquivocationRecord] =
    equivocationsTrackerList
      .map { record =>
        (record.equivocator, record.equivocationBaseBlockSeqNum) -> record.equivocationDetectedBlockHashes
      }
      .toMap
      .map {
        case ((equivocator, sequenceNumber), blockHashes) =>
          EquivocationRecord(equivocator, sequenceNumber, blockHashes)
      }
      .toSet

  private def calculateInvalidBlocksCrc[F[_]: Monad](
      invalidBlocks: List[BlockMetadata]
  ): Crc32[F] =
    Crc32[F](
      invalidBlocks
        .foldLeft(ByteString.EMPTY) {
          case (byteString, blockMetadata) =>
            val blockBytes = blockMetadata.toByteString
            byteString.concat(blockBytes.size().toByteString.concat(blockBytes))
        }
        .toByteArray
    )

  private def readInvalidBlocksLog[F[_]: Sync](
      randomAccessIO: RandomAccessIO[F]
  ): F[List[BlockMetadata]] =
    Monad[F].tailRecM[List[BlockMetadata], List[BlockMetadata]](List.empty) { result =>
      for {
        blockSizeOpt <- randomAccessIO.readInt
        result <- blockSizeOpt match {
                   case Some(blockSize) =>
                     val blockMetaBytes = Array.ofDim[Byte](blockSize)
                     for {
                       _             <- randomAccessIO.readFully(blockMetaBytes)
                       blockMetadata <- Sync[F].delay { BlockMetadata.fromBytes(blockMetaBytes) }
                     } yield (blockMetadata :: result).asLeft[List[BlockMetadata]]
                   case None =>
                     result.reverse.asRight[List[BlockMetadata]].pure[F]
                 }
      } yield result
    }

  private def truncateInvalidBlocksLog[F[_]: Sync](
      randomAccessIO: RandomAccessIO[F],
      invalidBlocks: List[BlockMetadata]
  ): F[Unit] = {
    val lastRecord = invalidBlocks.last
    // Size of the byte array (4 bytes) and the actual byte array
    val lastRecordSize: Long = IntSize + lastRecord.toByteString.size
    for {
      length <- randomAccessIO.length
      _      <- randomAccessIO.setLength(length - lastRecordSize)
    } yield ()
  }

  private def validateInvalidBlocks[F[_]: Sync](
      randomAccessIO: RandomAccessIO[F],
      readInvalidBlocksCrc: Long,
      invalidBlocks: List[BlockMetadata]
  ): F[(List[BlockMetadata], Crc32[F])] = {
    val fullCalculatedCrc = calculateInvalidBlocksCrc[F](invalidBlocks)
    Monad[F].ifM(fullCalculatedCrc.value.map(_ == readInvalidBlocksCrc))(
      (invalidBlocks, fullCalculatedCrc).pure[F],
      invalidBlocks match {
        case Nil =>
          Sync[F].raiseError[(List[BlockMetadata], Crc32[F])](
            InvalidBlocksIsCorrupted
          )
        case _ :: _ =>
          // Trying to delete the last log entry
          val withoutLastCalculatedCrc = calculateInvalidBlocksCrc[F](invalidBlocks.init)
          Monad[F].ifM(withoutLastCalculatedCrc.value.map(_ == readInvalidBlocksCrc))(
            for {
              _ <- truncateInvalidBlocksLog(randomAccessIO, invalidBlocks)
            } yield (invalidBlocks.init, withoutLastCalculatedCrc),
            Sync[F].raiseError[(List[BlockMetadata], Crc32[F])](
              InvalidBlocksIsCorrupted
            )
          )
      }
    )
  }

  private def extractChildMap(
      dataLookup: List[(BlockHash, BlockMetadata)]
  ): Map[BlockHash, Set[BlockHash]] =
    dataLookup.foldLeft(dataLookup.map(_._1 -> Set.empty[BlockHash]).toMap) {
      case (childMap, (_, blockMetadata)) =>
        blockMetadata.parents.foldLeft(childMap) {
          case (acc, p) =>
            val currentChildren = acc.getOrElse(p, Set.empty[BlockHash])
            acc.updated(p, currentChildren + blockMetadata.blockHash)
        }
    }

  private def extractTopoSort(
      dataLookup: List[(BlockHash, BlockMetadata)]
  ): Vector[Vector[BlockHash]] = {
    val blockMetadatas = dataLookup.map(_._2).toVector
    val indexedTopoSort =
      blockMetadatas.groupBy(_.blockNum).mapValues(_.map(_.blockHash)).toVector.sortBy(_._1)
    assert(indexedTopoSort.zipWithIndex.forall { case ((readI, _), i) => readI == i })
    indexedTopoSort.map(_._2)
  }

  private def loadCheckpoints[F[_]: Sync: Log: RaiseIOError](
      checkpointsDirPath: Path
  ): F[List[Checkpoint]] =
    for {
      _     <- makeDirectory[F](checkpointsDirPath)
      files <- listRegularFiles[F](checkpointsDirPath)
      checkpoints <- files.flatTraverse { filePath =>
                      filePath.getFileName.toString match {
                        case checkpointPattern(start, end) =>
                          List(Checkpoint(start.toLong, end.toLong, filePath, None)).pure[F]
                        case other =>
                          Log[F].warn(s"Ignoring file '$other': not a valid checkpoint name") *>
                            List.empty[Checkpoint].pure[F]
                      }
                    }
      sortedCheckpoints = checkpoints.sortBy(_.start)
      result <- if (sortedCheckpoints.headOption.forall(_.start == 0)) {
                 if (sortedCheckpoints.isEmpty ||
                     sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
                       case (current, next) => current.end == next.start
                     }) {
                   sortedCheckpoints.pure[F]
                 } else {
                   Sync[F].raiseError(CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.path)))
                 }
               } else {
                 Sync[F].raiseError(CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.path)))
               }
    } yield result

  private def loadBlockNumberIndexLmdbDbi[F[_]: Sync: Log: RaiseIOError](
      config: Config
  ): F[LmdbDbi[F, ByteBuffer]] =
    for {
      _ <- notExists[F](config.blockNumberIndexPath).ifM(
            makeDirectory[F](config.blockNumberIndexPath) >> ().pure[F],
            ().pure[F]
          )
      env <- Sync[F].delay {
              val flags = if (config.noTls) List(EnvFlags.MDB_NOTLS) else List.empty
              Env
                .create()
                .setMapSize(config.mapSize)
                .setMaxDbs(config.maxDbs)
                .setMaxReaders(config.maxReaders)
                .open(config.blockNumberIndexPath.toFile, flags: _*)
            }
      dbi <- Sync[F].delay {
              env.openDbi(s"block_dag_storage_block_number_index", MDB_CREATE)
            }
    } yield LmdbDbi[F, ByteBuffer](env, dbi)

  def create[F[_]: Concurrent: Sync: Log](
      config: Config
  ): F[BlockDagFileStorage[F]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]
    for {
      lock                  <- Semaphore[F](1)
      blockNumberIndex      <- loadBlockNumberIndexLmdbDbi(config)
      readLatestMessagesCrc <- readCrc[F](config.latestMessagesCrcPath)
      latestMessagesFileResource = Resource.make(
        RandomAccessIO.open[F](config.latestMessagesLogPath, RandomAccessIO.ReadWrite)
      )(_.close)
      latestMessagesResult <- latestMessagesFileResource.use { latestMessagesFile =>
                               for {
                                 latestMessagesReadResult <- readLatestMessagesData(
                                                              latestMessagesFile
                                                            )
                                 (latestMessagesList, logSize) = latestMessagesReadResult
                                 result <- validateLatestMessagesData[F](
                                            latestMessagesFile,
                                            readLatestMessagesCrc,
                                            config.latestMessagesCrcPath,
                                            latestMessagesList
                                          )
                                 (latestMessagesMap, calculatedLatestMessagesCrc) = result
                               } yield (latestMessagesMap, calculatedLatestMessagesCrc, logSize)
                             }
      (latestMessagesMap, calculatedLatestMessagesCrc, logSize) = latestMessagesResult
      readDataLookupCrc                                         <- readCrc[F](config.blockMetadataCrcPath)
      dataLookupFileResource = Resource.make(
        RandomAccessIO.open[F](config.blockMetadataLogPath, RandomAccessIO.ReadWrite)
      )(_.close)
      dataLookupResult <- dataLookupFileResource.use { randomAccessIO =>
                           for {
                             dataLookupList <- readDataLookupData(randomAccessIO)
                             result <- validateDataLookupData[F](
                                        randomAccessIO,
                                        readDataLookupCrc,
                                        config.blockMetadataCrcPath,
                                        dataLookupList
                                      )
                           } yield result
                         }
      (dataLookupList, calculatedDataLookupCrc) = dataLookupResult
      childMap                                  = extractChildMap(dataLookupList)
      topoSort                                  = extractTopoSort(dataLookupList)
      equivocationsTrackerFileResource = Resource.make(
        RandomAccessIO.open[F](config.equivocationsTrackerLogPath, RandomAccessIO.ReadWrite)
      )(_.close)
      readEquivocationsTrackerCrc <- readCrc[F](config.equivocationsTrackerCrcPath)
      equivocationsTrackerResult <- equivocationsTrackerFileResource.use {
                                     equivocationsTrackerFile =>
                                       for {
                                         equivocationsTrackerList <- readEquivocationsTrackerLog(
                                                                      equivocationsTrackerFile
                                                                    )
                                         result <- validateEquivocationsTrackerData(
                                                    equivocationsTrackerFile,
                                                    readEquivocationsTrackerCrc,
                                                    config.equivocationsTrackerCrcPath,
                                                    equivocationsTrackerList
                                                  )
                                       } yield result
                                   }
      (equivocationsTrackerList, calculatedEquivocationsTrackerCrc) = equivocationsTrackerResult
      equivocationsTracker                                          = squashEquivocationsTracker(equivocationsTrackerList)
      invalidBlocksFileResource = Resource.make(
        RandomAccessIO.open[F](config.invalidBlocksLogPath, RandomAccessIO.ReadWrite)
      )(_.close)
      readInvalidBlocksCrc <- readCrc[F](config.invalidBlocksCrcPath)
      invalidBlocksResult <- invalidBlocksFileResource.use { invalidBlocksFile =>
                              for {
                                invalidBlocksList <- readInvalidBlocksLog(
                                                      invalidBlocksFile
                                                    )
                                result <- validateInvalidBlocks(
                                           invalidBlocksFile,
                                           readInvalidBlocksCrc,
                                           invalidBlocksList
                                         )
                              } yield result
                            }
      (invalidBlocksList, calculatedInvalidBlocksCrc) = invalidBlocksResult
      sortedCheckpoints                               <- loadCheckpoints(config.checkpointsDirPath)
      latestMessagesLogOutputStream <- FileOutputStreamIO.open[F](
                                        config.latestMessagesLogPath,
                                        true
                                      )
      blockMetadataLogOutputStream <- FileOutputStreamIO.open[F](
                                       config.blockMetadataLogPath,
                                       true
                                     )
      equivocationsTrackerLogOutputStream <- FileOutputStreamIO.open[F](
                                              config.equivocationsTrackerLogPath,
                                              true
                                            )
      invalidBlocksLogOutputStream <- FileOutputStreamIO.open[F](
                                       config.invalidBlocksLogPath,
                                       true
                                     )
      state = BlockDagFileStorageState(
        latestMessages = latestMessagesMap,
        childMap = childMap,
        dataLookup = dataLookupList.toMap,
        topoSort = topoSort,
        equivocationsTracker = equivocationsTracker,
        invalidBlocks = invalidBlocksList.toSet,
        sortOffset = sortedCheckpoints.lastOption.map(_.end).getOrElse(0L),
        checkpoints = sortedCheckpoints,
        latestMessagesLogOutputStream = latestMessagesLogOutputStream,
        latestMessagesLogSize = logSize,
        latestMessagesCrc = calculatedLatestMessagesCrc,
        blockMetadataLogOutputStream = blockMetadataLogOutputStream,
        blockMetadataCrc = calculatedDataLookupCrc,
        equivocationsTrackerLogOutputStream = equivocationsTrackerLogOutputStream,
        equivocationsTrackerCrc = calculatedEquivocationsTrackerCrc,
        invalidBlocksLogOutputStream = invalidBlocksLogOutputStream,
        invalidBlocksCrc = calculatedInvalidBlocksCrc
      )
    } yield new BlockDagFileStorage[F](
      lock,
      blockNumberIndex,
      config.latestMessagesLogPath,
      config.latestMessagesCrcPath,
      config.latestMessagesLogMaxSizeFactor,
      config.blockMetadataLogPath,
      config.blockMetadataCrcPath,
      config.equivocationsTrackerLogPath,
      config.equivocationsTrackerCrcPath,
      config.invalidBlocksLogPath,
      config.invalidBlocksCrcPath,
      new AtomicMonadState[F, BlockDagFileStorageState[F]](AtomicAny(state))
    )
  }

  def createEmptyFromGenesis[F[_]: Concurrent: Sync: Log](
      config: Config,
      genesis: BlockMessage
  ): F[BlockDagFileStorage[F]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]
    for {
      lock                  <- Semaphore[F](1)
      blockNumberIndex      <- loadBlockNumberIndexLmdbDbi(config)
      _                     <- createFile[F](config.latestMessagesLogPath)
      _                     <- createFile[F](config.latestMessagesCrcPath)
      genesisBonds          = BlockMessageUtil.bonds(genesis)
      initialLatestMessages = genesisBonds.map(_.validator -> genesis.blockHash).toMap
      latestMessagesData = initialLatestMessages
        .foldLeft(ByteString.EMPTY) {
          case (byteString, (validator, blockHash)) =>
            byteString.concat(validator).concat(blockHash)
        }
        .toByteArray
      latestMessagesCrc = Crc32.empty[F]()
      _ <- initialLatestMessages.toList.traverse_ {
            case (validator, blockHash) =>
              latestMessagesCrc.update(validator.concat(blockHash).toByteArray)
          }
      latestMessagesCrcBytes <- latestMessagesCrc.bytes
      _                      <- writeToFile[F](config.latestMessagesLogPath, latestMessagesData)
      _                      <- writeToFile[F](config.latestMessagesCrcPath, latestMessagesCrcBytes)
      blockMetadataCrc       = Crc32.empty[F]()
      genesisByteString      = BlockMetadata.fromBlock(genesis, invalid = false).toByteString
      genesisData            = genesisByteString.size.toByteString.concat(genesisByteString).toByteArray
      _                      <- blockMetadataCrc.update(genesisData)
      blockMetadataCrcBytes  <- blockMetadataCrc.bytes
      _                      <- writeToFile[F](config.blockMetadataLogPath, genesisData)
      _                      <- writeToFile[F](config.blockMetadataCrcPath, blockMetadataCrcBytes)
      latestMessagesLogOutputStream <- FileOutputStreamIO.open[F](
                                        config.latestMessagesLogPath,
                                        true
                                      )
      blockMetadataLogOutputStream <- FileOutputStreamIO.open[F](
                                       config.blockMetadataLogPath,
                                       true
                                     )
      equivocationsTrackerLogOutputStream <- FileOutputStreamIO.open[F](
                                              config.equivocationsTrackerLogPath,
                                              true
                                            )
      invalidBlocksLogOutputStream <- FileOutputStreamIO.open[F](
                                       config.invalidBlocksLogPath,
                                       true
                                     )
      equivocationsTrackerCrc = Crc32.empty[F]()
      invalidBlocksCrc        = Crc32.empty[F]()
      state = BlockDagFileStorageState(
        latestMessages = initialLatestMessages,
        childMap = Map(genesis.blockHash   -> Set.empty[BlockHash]),
        dataLookup = Map(genesis.blockHash -> BlockMetadata.fromBlock(genesis, false)),
        topoSort = Vector(Vector(genesis.blockHash)),
        equivocationsTracker = Set.empty,
        invalidBlocks = Set.empty,
        sortOffset = 0L,
        checkpoints = List.empty,
        latestMessagesLogOutputStream = latestMessagesLogOutputStream,
        latestMessagesLogSize = initialLatestMessages.size,
        latestMessagesCrc = latestMessagesCrc,
        blockMetadataLogOutputStream = blockMetadataLogOutputStream,
        blockMetadataCrc = blockMetadataCrc,
        equivocationsTrackerLogOutputStream = equivocationsTrackerLogOutputStream,
        equivocationsTrackerCrc = equivocationsTrackerCrc,
        invalidBlocksLogOutputStream = invalidBlocksLogOutputStream,
        invalidBlocksCrc = invalidBlocksCrc
      )
    } yield new BlockDagFileStorage[F](
      lock,
      blockNumberIndex,
      config.latestMessagesLogPath,
      config.latestMessagesCrcPath,
      config.latestMessagesLogMaxSizeFactor,
      config.blockMetadataLogPath,
      config.blockMetadataCrcPath,
      config.equivocationsTrackerLogPath,
      config.equivocationsTrackerCrcPath,
      config.invalidBlocksLogPath,
      config.invalidBlocksCrcPath,
      new AtomicMonadState[F, BlockDagFileStorageState[F]](AtomicAny(state))
    )
  }
}
