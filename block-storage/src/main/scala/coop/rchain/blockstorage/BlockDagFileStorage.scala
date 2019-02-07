package coop.rchain.blockstorage

import java.nio.{BufferUnderflowException, ByteBuffer}
import java.nio.file.{Files, Path, StandardCopyOption}

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagFileStorage.{Checkpoint, CheckpointedDagInfo}
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.toStorageErrT
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.StorageError.{StorageErr, StorageErrT}
import coop.rchain.blockstorage.util.BlockMessageUtil.{blockNumber, bonds, parentHashes}
import coop.rchain.blockstorage.util.{BlockMessageUtil, Crc32, TopologicalSortUtil}
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.{AtomicMonadState, Log, LogSource}
import monix.execution.atomic.AtomicAny

import scala.ref.WeakReference
import scala.util.matching.Regex

private final case class BlockDagFileStorageState[F[_]: Sync](
    latestMessages: Map[Validator, BlockHash],
    childMap: Map[BlockHash, Set[BlockHash]],
    dataLookup: Map[BlockHash, BlockMetadata],
    topoSort: Vector[Vector[BlockHash]],
    sortOffset: Long,
    checkpoints: List[Checkpoint],
    latestMessagesLogOutputStream: FileOutputStreamIO[F],
    latestMessagesLogSize: Int,
    latestMessagesCrc: Crc32[F],
    blockMetadataLogOutputStream: FileOutputStreamIO[F],
    blockMetadataCrc: Crc32[F]
)

final class BlockDagFileStorage[F[_]: Concurrent: Sync: Log: BlockStore] private (
    lock: Semaphore[F],
    latestMessagesDataFilePath: Path,
    latestMessagesCrcFilePath: Path,
    latestMessagesLogMaxSizeFactor: Int,
    blockMetadataLogPath: Path,
    blockMetadataCrcPath: Path,
    state: MonadState[F, BlockDagFileStorageState[F]]
) extends BlockDagStorage[F] {
  private implicit val logSource = LogSource(BlockDagFileStorage.getClass)

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

  private[this] def setLatestMessages(v: Map[Validator, BlockHash]): F[Unit] =
    state.modify(s => s.copy(latestMessages = v))
  private[this] def setChildMap(v: Map[BlockHash, Set[BlockHash]]): F[Unit] =
    state.modify(s => s.copy(childMap = v))
  private[this] def setDataLookup(v: Map[BlockHash, BlockMetadata]): F[Unit] =
    state.modify(s => s.copy(dataLookup = v))
  private[this] def setTopoSort(v: Vector[Vector[BlockHash]]): F[Unit] =
    state.modify(s => s.copy(topoSort = v))
  private[this] def setSortOffset(v: Long): F[Unit] =
    state.modify(s => s.copy(sortOffset = v))
  private[this] def setCheckpoints(v: List[Checkpoint]): F[Unit] =
    state.modify(s => s.copy(checkpoints = v))
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
  private[this] def modifySortOffset(f: Long => Long): F[Unit] =
    state.modify(s => s.copy(sortOffset = f(s.sortOffset)))
  private[this] def modifyCheckpoints(f: List[Checkpoint] => List[Checkpoint]): F[Unit] =
    state.modify(s => s.copy(checkpoints = f(s.checkpoints)))
  private[this] def modifyLatestMessagesLogOutputStream(
      f: FileOutputStreamIO[F] => FileOutputStreamIO[F]
  ): F[Unit] =
    state.modify(s => s.copy(latestMessagesLogOutputStream = f(s.latestMessagesLogOutputStream)))
  private[this] def modifyLatestMessagesLogSize(f: Int => Int): F[Unit] =
    state.modify(s => s.copy(latestMessagesLogSize = f(s.latestMessagesLogSize)))
  private[this] def modifyLatestMessagesCrc(f: Crc32[F] => Crc32[F]): F[Unit] =
    state.modify(s => s.copy(latestMessagesCrc = f(s.latestMessagesCrc)))
  private[this] def modifyBlockMetadataLogOutputStream(
      f: FileOutputStreamIO[F] => FileOutputStreamIO[F]
  ): F[Unit] =
    state.modify(s => s.copy(blockMetadataLogOutputStream = f(s.blockMetadataLogOutputStream)))
  private[this] def modifyBlockMetadataCrc(f: Crc32[F] => Crc32[F]): F[Unit] =
    state.modify(s => s.copy(blockMetadataCrc = f(s.blockMetadataCrc)))

  private case class FileDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]],
      sortOffset: Long
  ) extends BlockDagRepresentation[F] {
    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      for {
        result <- childMap.get(blockHash) match {
                   case Some(children) =>
                     Option(children).pure[F]
                   case None =>
                     for {
                       blockOpt <- BlockStore[F].get(blockHash)
                       result <- blockOpt match {
                                  case Some(block) =>
                                    val number = blockNumber(block)
                                    if (number >= sortOffset) {
                                      none[Set[BlockHash]].pure[F]
                                    } else {
                                      lock.withPermit(
                                        loadCheckpoint(number).flatMap {
                                          case Right(oldDagInfo) =>
                                            oldDagInfo.childMap.get(blockHash).pure[F]
                                          case Left(error) =>
                                            Log[F].error(error.message) *>
                                              none[Set[BlockHash]].pure[F]
                                        }
                                      )
                                    }
                                  case None => none[Set[BlockHash]].pure[F]
                                }
                     } yield result
                 }
      } yield result
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup
        .get(blockHash)
        .fold(
          BlockStore[F].get(blockHash).map(_.map(BlockMetadata.fromBlock))
        )(blockMetadata => Option(blockMetadata).pure[F])
    def contains(blockHash: BlockHash): F[Boolean] =
      dataLookup.get(blockHash).fold(BlockStore[F].contains(blockHash))(_ => true.pure[F])
    def topoSort(startBlockNumber: Long): F[StorageErr[Vector[Vector[BlockHash]]]] =
      if (startBlockNumber >= sortOffset) {
        val offset = startBlockNumber - sortOffset
        assert(offset.isValidInt)
        topoSortVector.drop(offset.toInt).asRight[StorageError].pure[F]
      } else if (sortOffset - startBlockNumber + topoSortVector.length < Int.MaxValue) { // Max Vector length
        lock.withPermit {
          type StorageErrTF[A] = StorageErrT[F, A]
          (for {
            checkpoints          <- EitherT.liftF[F, StorageError, List[Checkpoint]](getCheckpoints)
            checkpointsWithIndex = checkpoints.zipWithIndex
            checkpointsToLoad    = checkpointsWithIndex.filter(startBlockNumber < _._1.end)
            checkpointsDagInfos <- checkpointsToLoad.traverse[StorageErrTF, CheckpointedDagInfo] {
                                    case (startingCheckpoint, index) =>
                                      EitherT(loadCheckpointDagInfo(startingCheckpoint, index))
                                  }
            topoSortPrefix = checkpointsDagInfos.toVector.flatMap { checkpointsDagInfo =>
              val offset = startBlockNumber - checkpointsDagInfo.sortOffset
              // offset is always a valid Int since the method result's length was validated before
              checkpointsDagInfo.topoSort.drop(offset.toInt) // negative drops are ignored
            }
            result = topoSortPrefix ++ topoSortVector
          } yield result).value
        }
      } else {
        (TopoSortLengthIsTooBig(sortOffset - startBlockNumber + topoSortVector.length): StorageError)
          .asLeft[Vector[Vector[BlockHash]]]
          .pure[F]
      }
    def topoSortTail(tailLength: Int): F[StorageErr[Vector[Vector[BlockHash]]]] = {
      val startBlockNumber = Math.max(0L, sortOffset - (tailLength - topoSortVector.length))
      topoSort(startBlockNumber)
    }
    def deriveOrdering(startBlockNumber: Long): F[StorageErr[Ordering[BlockMetadata]]] =
      topoSort(startBlockNumber).map { topologicalSortingEither =>
        topologicalSortingEither.right.map { topologicalSorting =>
          val order = topologicalSorting.flatten.zipWithIndex.toMap
          Ordering.by(b => order(b.blockHash))
        }
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
  }

  private def loadDagInfo(checkpoint: Checkpoint): F[StorageErr[CheckpointedDagInfo]] =
    (for {
      checkpointRandomAccessIo <- toStorageErrT(
                                   RandomAccessIO.open[F](checkpoint.path, RandomAccessIO.Read)
                                 )
      blockMetadataList <- EitherT(BlockDagFileStorage.readDataLookupData(checkpointRandomAccessIo))
      _                 <- toStorageErrT(checkpointRandomAccessIo.close)
      dataLookup        = blockMetadataList.toMap
      childMap          = BlockDagFileStorage.extractChildMap(blockMetadataList)
      topoSort          = BlockDagFileStorage.extractTopoSort(blockMetadataList)
    } yield CheckpointedDagInfo(childMap, dataLookup, topoSort, checkpoint.start)).value

  private def loadCheckpointDagInfo(
      checkpoint: Checkpoint,
      index: Int
  ): F[StorageErr[CheckpointedDagInfo]] =
    checkpoint.dagInfo.flatMap(_.get) match {
      case Some(dagInfo) =>
        dagInfo.asRight[StorageError].pure[F]
      case None =>
        loadDagInfo(checkpoint).flatMap {
          case Right(loadedDagInfo) =>
            val newCheckpoint = checkpoint.copy(dagInfo = Some(WeakReference(loadedDagInfo)))
            modifyCheckpoints(_.patch(index, List(newCheckpoint), 1)) *>
              loadedDagInfo.asRight[StorageError].pure[F]
          case other =>
            other.pure[F]
        }
    }

  private def loadCheckpoint(offset: Long): F[StorageErr[CheckpointedDagInfo]] =
    for {
      checkpoints <- getCheckpoints
      neededCheckpoint = checkpoints.zipWithIndex.find {
        case (c, _) => c.start <= offset && offset < c.end
      }
      result <- neededCheckpoint match {
                 case None =>
                   (CheckpointDoesNotExist(offset): StorageError)
                     .asLeft[CheckpointedDagInfo]
                     .pure[F]
                 case Some((checkpoint, i)) =>
                   loadCheckpointDagInfo(checkpoint, i)
               }
    } yield result

  private def updateLatestMessagesFile(
      validators: List[Validator],
      blockHash: BlockHash
  ): F[StorageErr[Unit]] = {
    type StorageErrTF[A] = StorageErrT[F, A]
    (for {
      latestMessagesCrc <- EitherT.liftF[F, StorageError, Crc32[F]](getLatestMessagesCrc)
      _ <- validators.traverse_[StorageErrTF, Unit] { validator =>
            val toAppend = validator.concat(blockHash).toByteArray
            for {
              latestMessagesLogOutputStream <- EitherT
                                                .liftF[F, StorageError, FileOutputStreamIO[F]](
                                                  getLatestMessagesLogOutputStream
                                                )
              _ <- toStorageErrT(latestMessagesLogOutputStream.write(toAppend))
              _ <- toStorageErrT(latestMessagesLogOutputStream.flush)
              _ <- EitherT(latestMessagesCrc.update(toAppend).flatMap { _ =>
                    updateLatestMessagesCrcFile(latestMessagesCrc)
                  })
            } yield ()
          }
      _ <- EitherT.liftF[F, StorageError, Unit](modifyLatestMessagesLogSize(_ + 1))
    } yield ()).value
  }

  private def updateLatestMessagesCrcFile(newCrc: Crc32[F]): F[StorageErr[Unit]] =
    (for {
      newCrcBytes <- EitherT.liftF[F, StorageError, Array[Byte]](newCrc.bytes)
      tmpCrc <- toStorageErrT(
                 createTemporaryFile("rchain-block-dag-file-storage-latest-messages-", "-crc")
               )
      _ <- toStorageErrT(writeToFile(tmpCrc, newCrcBytes))
      _ <- toStorageErrT(
            moveFile(
              tmpCrc,
              latestMessagesCrcFilePath,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
          )
    } yield ()).value

  private def squashLatestMessagesDataFile(): F[StorageErr[Unit]] =
    (for {
      latestMessages <- EitherT.liftF[F, StorageError, Map[Validator, BlockHash]](
                         getLatestMessages
                       )
      latestMessagesLogOutputStream <- EitherT.liftF(getLatestMessagesLogOutputStream)
      _                             <- toStorageErrT(latestMessagesLogOutputStream.close)
      tmpSquashedData <- toStorageErrT(
                          createTemporaryFile(
                            "rchain-block-dag-store-latest-messages-",
                            "-squashed-data"
                          )
                        )
      tmpSquashedCrc <- toStorageErrT(
                         createTemporaryFile(
                           "rchain-block-dag-store-latest-messages-",
                           "-squashed-crc"
                         )
                       )
      dataByteBuffer = ByteBuffer.allocate(64 * latestMessages.size)
      _ <- EitherT.liftF(latestMessages.toList.traverse_ {
            case (validator, blockHash) =>
              Sync[F].delay {
                dataByteBuffer.put(validator.toByteArray)
                dataByteBuffer.put(blockHash.toByteArray)
              }
          })
      _                <- toStorageErrT(writeToFile(tmpSquashedData, dataByteBuffer.array()))
      squashedCrc      = Crc32.empty[F]()
      _                <- EitherT.liftF(squashedCrc.update(dataByteBuffer.array()))
      squashedCrcBytes <- EitherT.liftF(squashedCrc.bytes)
      _                <- toStorageErrT(writeToFile(tmpSquashedCrc, squashedCrcBytes))
      _ <- toStorageErrT(
            moveFile(
              tmpSquashedData,
              latestMessagesDataFilePath,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
          )
      _ <- toStorageErrT(
            moveFile(
              tmpSquashedCrc,
              latestMessagesCrcFilePath,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
          )
      newLatestMessageLogOutputStream <- toStorageErrT(
                                          FileOutputStreamIO.open(latestMessagesDataFilePath, true)
                                        )
      _ <- EitherT.liftF[F, StorageError, Unit](
            setLatestMessagesLogOutputStream(newLatestMessageLogOutputStream)
          )
      _ <- EitherT.liftF[F, StorageError, Unit](setLatestMessagesCrc(squashedCrc))
      _ <- EitherT.liftF[F, StorageError, Unit](setLatestMessagesLogSize(0))
    } yield ()).value

  private def squashLatestMessagesDataFileIfNeeded(): F[StorageErr[Unit]] =
    for {
      latestMessages        <- getLatestMessages
      latestMessagesLogSize <- getLatestMessagesLogSize
      result <- if (latestMessagesLogSize > latestMessages.size * latestMessagesLogMaxSizeFactor) {
                 squashLatestMessagesDataFile()
               } else {
                 ().asRight[StorageError].pure[F]
               }
    } yield result

  private def updateDataLookupFile(blockMetadata: BlockMetadata): F[StorageErr[Unit]] =
    (for {
      dataLookupCrc          <- EitherT.liftF[F, StorageError, Crc32[F]](getBlockMetadataCrc)
      blockBytes             = blockMetadata.toByteString
      toAppend               = blockBytes.size.toByteString.concat(blockBytes).toByteArray
      dataLookupOutputStream <- EitherT.liftF(getBlockMetadataLogOutputStream)
      _                      <- toStorageErrT(dataLookupOutputStream.write(toAppend))
      _                      <- toStorageErrT(dataLookupOutputStream.flush)
      _                      <- EitherT.liftF(dataLookupCrc.update(toAppend))
      _                      <- EitherT(updateDataLookupCrcFile(dataLookupCrc))
    } yield ()).value

  private def updateDataLookupCrcFile(newCrc: Crc32[F]): F[StorageErr[Unit]] =
    (for {
      newCrcBytes <- EitherT.liftF[F, StorageError, Array[Byte]](newCrc.bytes)
      tmpCrc <- toStorageErrT(
                 createTemporaryFile("rchain-block-dag-file-storage-data-lookup-", "-crc")
               )
      _ <- toStorageErrT(writeToFile(tmpCrc, newCrcBytes))
      _ <- toStorageErrT(
            moveFile(
              tmpCrc,
              blockMetadataCrcPath,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
          )
    } yield ()).value

  private def representation: F[BlockDagRepresentation[F]] =
    for {
      latestMessages <- getLatestMessages
      childMap       <- getChildMap
      dataLookup     <- getDataLookup
      topoSort       <- getTopoSort
      sortOffset     <- getSortOffset
    } yield FileDagRepresentation(latestMessages, childMap, dataLookup, topoSort, sortOffset)

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(block: BlockMessage): F[StorageErr[BlockDagRepresentation[F]]] =
    lock.withPermit(
      for {
        alreadyStored <- getDataLookup.map(_.contains(block.blockHash))
        insertion <- if (alreadyStored) {
                      Log[F].warn(
                        s"Block ${Base16.encode(block.blockHash.toByteArray)} is already stored"
                      ) *>
                        ().asRight[StorageError].pure[F]
                    } else {
                      for {
                        _             <- squashLatestMessagesDataFileIfNeeded()
                        blockMetadata = BlockMetadata.fromBlock(block)
                        _             = assert(block.blockHash.size == 32)
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
                        _ <- modifyTopoSort(
                              topoSort => TopologicalSortUtil.update(topoSort, 0L, block)
                            )
                        //Block which contains newly bonded validators will not
                        //have those validators in its justification
                        newValidators = bonds(block)
                          .map(_.validator)
                          .toSet
                          .diff(block.justifications.map(_.validator).toSet)
                        newValidatorsWithSenderEither <- if (block.sender.isEmpty) {
                                                          // Ignore empty sender for special cases such as genesis block
                                                          Log[F].warn(
                                                            s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                          ) *> newValidators
                                                            .asRight[StorageError]
                                                            .pure[F]
                                                        } else if (block.sender.size() == 32) {
                                                          (newValidators + block.sender)
                                                            .asRight[StorageError]
                                                            .pure[F]
                                                        } else {
                                                          (BlockSenderIsMalformed(block): StorageError)
                                                            .asLeft[Set[ByteString]]
                                                            .pure[F]
                                                        }
                        _ <- newValidatorsWithSenderEither.traverse { newValidatorsWithSender =>
                              modifyLatestMessages { latestMessages =>
                                newValidatorsWithSender.foldLeft(latestMessages) {
                                  //Update new validators with block in which
                                  //they were bonded (i.e. this block)
                                  case (acc, v) => acc.updated(v, block.blockHash)
                                }
                              }
                            }
                        _ <- updateLatestMessagesFile(
                              (newValidators + block.sender).toList,
                              block.blockHash
                            )
                        _ <- updateDataLookupFile(blockMetadata)
                      } yield newValidatorsWithSenderEither.map(_ => ())
                    }
        dag <- representation
      } yield insertion.map(_ => dag)
    )

  def checkpoint(): F[StorageErr[Unit]] =
    ().asRight[StorageError].pure[F]

  def clear(): F[StorageErr[Unit]] =
    lock.withPermit(
      (for {
        latestMessagesLogOutputStream <- EitherT.liftF[F, StorageError, FileOutputStreamIO[F]](
                                          getLatestMessagesLogOutputStream
                                        )
        _ <- toStorageErrT(latestMessagesLogOutputStream.close)
        blockMetadataLogOutputStream <- EitherT.liftF[F, StorageError, FileOutputStreamIO[F]](
                                         getBlockMetadataLogOutputStream
                                       )
        _                    <- toStorageErrT(blockMetadataLogOutputStream.close)
        _                    <- toStorageErrT(writeToFile(latestMessagesDataFilePath, Array.emptyByteArray))
        _                    <- toStorageErrT(writeToFile(blockMetadataLogPath, Array.emptyByteArray))
        newLatestMessagesCrc = Crc32.empty[F]()
        newLatestMessagesCrcBytes <- EitherT.liftF[F, StorageError, Array[Byte]](
                                      newLatestMessagesCrc.bytes
                                    )
        _                   <- toStorageErrT(writeToFile(latestMessagesCrcFilePath, newLatestMessagesCrcBytes))
        newBlockMetadataCrc = Crc32.empty[F]()
        newBlockMetadataCrcBytes <- EitherT.liftF[F, StorageError, Array[Byte]](
                                     newBlockMetadataCrc.bytes
                                   )
        _ <- toStorageErrT(writeToFile(blockMetadataCrcPath, newBlockMetadataCrcBytes))
        _ <- EitherT.liftF[F, StorageError, Unit](setDataLookup(Map.empty))
        _ <- EitherT.liftF[F, StorageError, Unit](setChildMap(Map.empty))
        _ <- EitherT.liftF[F, StorageError, Unit](setTopoSort(Vector.empty))
        _ <- EitherT.liftF[F, StorageError, Unit](setLatestMessages(Map.empty))
        newLatestMessageLogOutputStream <- toStorageErrT(
                                            FileOutputStreamIO
                                              .open(latestMessagesDataFilePath, true)
                                          )
        _ <- EitherT.liftF[F, StorageError, Unit](
              setLatestMessagesLogOutputStream(newLatestMessageLogOutputStream)
            )
        newBlockMetadataLogOutputStream <- toStorageErrT(
                                            FileOutputStreamIO.open(blockMetadataLogPath, true)
                                          )
        _ <- EitherT.liftF[F, StorageError, Unit](
              setBlockMetadataLogOutputStream(newBlockMetadataLogOutputStream)
            )
        _ <- EitherT.liftF[F, StorageError, Unit](setLatestMessagesLogSize(0))
        _ <- EitherT.liftF[F, StorageError, Unit](setLatestMessagesCrc(newLatestMessagesCrc))
        _ <- EitherT.liftF[F, StorageError, Unit](setBlockMetadataCrc(newBlockMetadataCrc))
      } yield ()).value
    )

  def close(): F[StorageErr[Unit]] =
    lock.withPermit(
      (for {
        latestMessagesLogOutputStream <- EitherT.liftF[F, StorageError, FileOutputStreamIO[F]](
                                          getLatestMessagesLogOutputStream
                                        )
        _                            <- toStorageErrT(latestMessagesLogOutputStream.close)
        blockMetadataLogOutputStream <- EitherT.liftF(getBlockMetadataLogOutputStream)
        _                            <- toStorageErrT(blockMetadataLogOutputStream.close)
      } yield ()).value
    )
}

object BlockDagFileStorage {
  private implicit val logSource       = LogSource(BlockDagFileStorage.getClass)
  private val checkpointPattern: Regex = "([0-9]+)-([0-9]+)".r

  final case class Config(
      latestMessagesLogPath: Path,
      latestMessagesCrcPath: Path,
      blockMetadataLogPath: Path,
      blockMetadataCrcPath: Path,
      checkpointsDirPath: Path,
      latestMessagesLogMaxSizeFactor: Int = 10
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

  private def wrapIoError(error: IOError): StorageError =
    WrappedIOError(error)

  private def readCrc[F[_]: Sync: Log](crcPath: Path): F[StorageErr[Long]] =
    (for {
      _          <- toStorageErrT(createNewFile(crcPath))
      bytes      <- toStorageErrT(readAllBytesFromFile(crcPath))
      byteBuffer = ByteBuffer.wrap(bytes)
      result <- EitherT(Sync[F].delay { byteBuffer.getLong() }.attempt.flatMap {
                 case Left(_: BufferUnderflowException) =>
                   for {
                     _ <- Log[F].warn(s"CRC file $crcPath did not contain a valid CRC value")
                   } yield 0L.asRight[StorageError]
                 case Left(e) =>
                   wrapIoError(UnexpectedIOError(e)).asLeft[Long].pure[F]
                 case Right(value) =>
                   value.asRight[StorageError].pure[F]
               })
    } yield result).value

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
  ): F[StorageErr[(List[(Validator, BlockHash)], Int)]] = {
    def readRec(
        result: List[(Validator, BlockHash)],
        logSize: Int
    ): F[StorageErr[(List[(Validator, BlockHash)], Int)]] = {
      val validatorPk = Array.fill[Byte](32)(0)
      val blockHash   = Array.fill[Byte](32)(0)
      for {
        validatorPkRead <- randomAccessIO.readFully(validatorPk)
        blockHashRead   <- randomAccessIO.readFully(blockHash)
        result <- (validatorPkRead, blockHashRead) match {
                   case (Right(_), Right(_)) =>
                     val pair = (ByteString.copyFrom(validatorPk), ByteString.copyFrom(blockHash))
                     readRec(
                       pair :: result,
                       logSize + 1
                     )
                   case (Left(_: EndOfFile), Left(_: EndOfFile)) =>
                     (result.reverse, logSize).asRight[StorageError].pure[F]
                   case (Right(_), Left(e: EndOfFile)) =>
                     for {
                       _ <- Log[F].error("Latest messages log is malformed")
                     } yield wrapIoError(e).asLeft[(List[(Validator, BlockHash)], Int)]
                   case (Right(_), Left(e)) =>
                     wrapIoError(e).asLeft[(List[(Validator, BlockHash)], Int)].pure[F]
                   case (Left(e), _) =>
                     wrapIoError(e).asLeft[(List[(Validator, BlockHash)], Int)].pure[F]
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
  ): F[StorageErr[(Map[Validator, BlockHash], Crc32[F])]] = {
    val fullCalculatedCrc = calculateLatestMessagesCrc[F](latestMessagesList)
    (for {
      fullCalculatedCrcValue <- EitherT.liftF[F, StorageError, Long](fullCalculatedCrc.value)
      result <- if (fullCalculatedCrcValue == readLatestMessagesCrc) {
                 EitherT.rightT[F, StorageError]((latestMessagesList.toMap, fullCalculatedCrc))
               } else {
                 val withoutLastCalculatedCrc =
                   calculateLatestMessagesCrc[F](latestMessagesList.init)
                 for {
                   withoutLastCalculatedCrcValue <- EitherT.liftF[F, StorageError, Long](
                                                     withoutLastCalculatedCrc.value
                                                   )
                   result <- if (withoutLastCalculatedCrcValue == readLatestMessagesCrc) {
                              for {
                                length <- toStorageErrT(latestMessagesRaf.length)
                                _      <- toStorageErrT(latestMessagesRaf.setLength(length - 64))
                              } yield (latestMessagesList.init.toMap, withoutLastCalculatedCrc)
                            } else {
                              // TODO: Restore latest messages from the persisted DAG
                              toStorageErrT(latestMessagesRaf.setLength(0)) *> EitherT
                                .rightT[F, StorageError](
                                  (Map.empty[Validator, BlockHash], Crc32.empty[F]())
                                )
                            }
                 } yield result
               }
    } yield result).value
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
  ): F[StorageErr[List[(BlockHash, BlockMetadata)]]] = {
    def readRec(
        result: List[(BlockHash, BlockMetadata)]
    ): F[StorageErr[List[(BlockHash, BlockMetadata)]]] =
      for {
        blockSizeEither <- randomAccessIO.readInt
        result <- blockSizeEither match {
                   case Right(blockSize) =>
                     val blockMetaBytes = Array.ofDim[Byte](blockSize)
                     (for {
                       _ <- toStorageErrT(randomAccessIO.readFully(blockMetaBytes))
                       blockMetadata <- EitherT.liftF(Sync[F].delay {
                                         BlockMetadata.fromBytes(blockMetaBytes)
                                       })
                       result <- EitherT(
                                  readRec((blockMetadata.blockHash -> blockMetadata) :: result)
                                )
                     } yield result).value
                   case Left(_: EndOfFile) =>
                     result.reverse.asRight[StorageError].pure[F]
                   case Left(other) =>
                     WrappedIOError(other).asLeft[List[(BlockHash, BlockMetadata)]].pure[F]
                 }
      } yield result
    readRec(List.empty)
  }

  private def validateDataLookupData[F[_]: Sync](
      dataLookupRandomAccessFile: RandomAccessIO[F],
      readDataLookupCrc: Long,
      dataLookupCrcPath: Path,
      dataLookupList: List[(BlockHash, BlockMetadata)]
  ): F[StorageErr[(List[(BlockHash, BlockMetadata)], Crc32[F])]] = {
    val fullCalculatedCrc = calculateDataLookupCrc[F](dataLookupList)
    EitherT
      .liftF[F, StorageError, Long](fullCalculatedCrc.value)
      .flatMap { fullCalculatedCrcValue =>
        if (fullCalculatedCrcValue == readDataLookupCrc) {
          EitherT.rightT[F, StorageError]((dataLookupList, fullCalculatedCrc))
        } else if (dataLookupList.nonEmpty) {
          val withoutLastCalculatedCrc = calculateDataLookupCrc[F](dataLookupList.init)
          EitherT.liftF[F, StorageError, Long](withoutLastCalculatedCrc.value).flatMap {
            withoutLastCalculatedCrcValue =>
              if (withoutLastCalculatedCrcValue == readDataLookupCrc) {
                val byteString                    = dataLookupList.last._2.toByteString
                val lastDataLookupEntrySize: Long = 4L + byteString.size()
                for {
                  length <- toStorageErrT(dataLookupRandomAccessFile.length)
                  _ <- toStorageErrT(
                        dataLookupRandomAccessFile.setLength(length - lastDataLookupEntrySize)
                      )
                } yield (dataLookupList.init, withoutLastCalculatedCrc)
              } else {
                // TODO: Restore data lookup from block storage
                toStorageErrT(dataLookupRandomAccessFile.setLength(0)) *>
                  EitherT.rightT[F, StorageError](
                    (List.empty[(BlockHash, BlockMetadata)], Crc32.empty[F]())
                  )
              }
          }
        } else {
          // TODO: Restore data lookup from block storage
          toStorageErrT(dataLookupRandomAccessFile.setLength(0)) *>
            EitherT.rightT[F, StorageError](
              (List.empty[(BlockHash, BlockMetadata)], Crc32.empty[F]())
            )
        }
      }
      .value
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
    val indexedTopoSort =
      dataLookup.map(_._2).toVector.groupBy(_.blockNum).mapValues(_.map(_.blockHash)).toVector
    assert(indexedTopoSort.zipWithIndex.forall { case ((readI, _), i) => readI == i })
    indexedTopoSort.map(_._2)
  }

  private def loadCheckpoints[F[_]: Sync: Log](
      checkpointsDirPath: Path
  ): F[StorageErr[List[Checkpoint]]] =
    (for {
      filesList <- toStorageErrT(listFiles(checkpointsDirPath))
      checkpoints <- EitherT.liftF[F, StorageError, List[Checkpoint]](filesList.flatTraverse {
                      filePath =>
                        filePath.getFileName.toString match {
                          case checkpointPattern(start, end) =>
                            List(Checkpoint(start.toLong, end.toLong, filePath, None)).pure[F]
                          case other =>
                            Log[F].warn(s"Ignoring file '$other': not a valid checkpoint name") *>
                              List.empty[Checkpoint].pure[F]
                        }
                    })
      sortedCheckpoints = checkpoints.sortBy(_.start)
      result <- EitherT.fromEither[F](if (sortedCheckpoints.headOption.forall(_.start == 0)) {
                 if (sortedCheckpoints.isEmpty ||
                     sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
                       case (current, next) => current.end == next.start
                     }) {
                   sortedCheckpoints.asRight[StorageError]
                 } else {
                   CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.path))
                     .asLeft[List[Checkpoint]]
                 }
               } else {
                 CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.path))
                   .asLeft[List[Checkpoint]]
               })
    } yield result).value

  def create[F[_]: Concurrent: Sync: Capture: Log: BlockStore](
      config: Config
  ): F[StorageErr[BlockDagFileStorage[F]]] =
    (for {
      lock                  <- EitherT.liftF[F, StorageError, Semaphore[F]](Semaphore[F](1))
      readLatestMessagesCrc <- EitherT(readCrc[F](config.latestMessagesCrcPath))
      latestMessagesFile <- toStorageErrT(
                             RandomAccessIO
                               .open[F](config.latestMessagesLogPath, RandomAccessIO.ReadWrite)
                           )
      latestMessagesReadResult      <- EitherT(readLatestMessagesData[F](latestMessagesFile))
      (latestMessagesList, logSize) = latestMessagesReadResult
      result <- EitherT(
                 validateLatestMessagesData[F](
                   latestMessagesFile,
                   readLatestMessagesCrc,
                   config.latestMessagesCrcPath,
                   latestMessagesList
                 )
               )
      _                                                <- toStorageErrT(latestMessagesFile.close)
      (latestMessagesMap, calculatedLatestMessagesCrc) = result
      readDataLookupCrc                                <- EitherT(readCrc[F](config.blockMetadataCrcPath))
      dataLookupFile <- toStorageErrT(
                         RandomAccessIO
                           .open[F](config.blockMetadataLogPath, RandomAccessIO.ReadWrite)
                       )
      dataLookupList <- EitherT(readDataLookupData(dataLookupFile))
      dataLookupResult <- EitherT(
                           validateDataLookupData[F](
                             dataLookupFile,
                             readDataLookupCrc,
                             config.blockMetadataCrcPath,
                             dataLookupList
                           )
                         )
      _                                         <- toStorageErrT(dataLookupFile.close)
      (dataLookupList, calculatedDataLookupCrc) = dataLookupResult
      childMap                                  = extractChildMap(dataLookupList)
      topoSort                                  = extractTopoSort(dataLookupList)
      sortedCheckpoints                         <- EitherT(loadCheckpoints(config.checkpointsDirPath))
      latestMessagesLogOutputStream <- toStorageErrT(
                                        FileOutputStreamIO.open(config.latestMessagesLogPath, true)
                                      )
      blockMetadataLogOutputStream <- toStorageErrT(
                                       FileOutputStreamIO.open(config.blockMetadataLogPath, true)
                                     )
      state = BlockDagFileStorageState(
        latestMessagesMap,
        childMap,
        dataLookupList.toMap,
        topoSort,
        sortedCheckpoints.lastOption.map(_.end).getOrElse(0L),
        sortedCheckpoints,
        latestMessagesLogOutputStream,
        logSize,
        calculatedLatestMessagesCrc,
        blockMetadataLogOutputStream,
        calculatedDataLookupCrc
      )
    } yield
      new BlockDagFileStorage[F](
        lock,
        config.latestMessagesLogPath,
        config.latestMessagesCrcPath,
        config.latestMessagesLogMaxSizeFactor,
        config.blockMetadataLogPath,
        config.blockMetadataCrcPath,
        new AtomicMonadState[F, BlockDagFileStorageState[F]](AtomicAny(state))
      )).value

  def createEmptyFromGenesis[F[_]: Concurrent: Sync: Capture: Log: BlockStore](
      config: Config,
      genesis: BlockMessage
  ): F[StorageErr[BlockDagFileStorage[F]]] =
    (for {
      lock                  <- EitherT.liftF[F, StorageError, Semaphore[F]](Semaphore[F](1))
      _                     <- toStorageErrT(createFile(config.latestMessagesLogPath))
      _                     <- toStorageErrT(createFile(config.latestMessagesCrcPath))
      genesisBonds          = BlockMessageUtil.bonds(genesis)
      initialLatestMessages = genesisBonds.map(_.validator -> genesis.blockHash).toMap
      latestMessagesData = initialLatestMessages
        .foldLeft(ByteString.EMPTY) {
          case (byteString, (validator, blockHash)) =>
            byteString.concat(validator).concat(blockHash)
        }
        .toByteArray
      latestMessagesCrc = Crc32.empty[F]()
      _ <- EitherT.liftF(initialLatestMessages.toList.traverse_ {
            case (validator, blockHash) =>
              latestMessagesCrc.update(validator.concat(blockHash).toByteArray)
          })
      latestMessagesCrcBytes <- EitherT.liftF(latestMessagesCrc.bytes)
      _                      <- toStorageErrT(writeToFile(config.latestMessagesLogPath, latestMessagesData))
      _                      <- toStorageErrT(writeToFile(config.latestMessagesCrcPath, latestMessagesCrcBytes))
      blockMetadataCrc       = Crc32.empty[F]()
      genesisByteString      = genesis.toByteString
      genesisData            = genesisByteString.size.toByteString.concat(genesisByteString).toByteArray
      _                      <- EitherT.liftF(blockMetadataCrc.update(genesisData))
      blockMetadataCrcBytes  <- EitherT.liftF(blockMetadataCrc.bytes)
      _                      <- toStorageErrT(writeToFile(config.blockMetadataLogPath, genesisData))
      _                      <- toStorageErrT(writeToFile(config.blockMetadataCrcPath, blockMetadataCrcBytes))
      latestMessagesLogOutputStream <- toStorageErrT(
                                        FileOutputStreamIO.open(config.latestMessagesLogPath, true)
                                      )
      blockMetadataLogOutputStream <- toStorageErrT(
                                       FileOutputStreamIO.open(config.blockMetadataLogPath, true)
                                     )
      state = BlockDagFileStorageState(
        initialLatestMessages,
        Map(genesis.blockHash -> Set.empty[BlockHash]),
        Map(genesis.blockHash -> BlockMetadata.fromBlock(genesis)),
        Vector(Vector(genesis.blockHash)),
        0L,
        List.empty,
        latestMessagesLogOutputStream,
        initialLatestMessages.size,
        latestMessagesCrc,
        blockMetadataLogOutputStream,
        blockMetadataCrc
      )
    } yield
      new BlockDagFileStorage[F](
        lock,
        config.latestMessagesLogPath,
        config.latestMessagesCrcPath,
        config.latestMessagesLogMaxSizeFactor,
        config.blockMetadataLogPath,
        config.blockMetadataCrcPath,
        new AtomicMonadState[F, BlockDagFileStorageState[F]](AtomicAny(state))
      )).value
}
