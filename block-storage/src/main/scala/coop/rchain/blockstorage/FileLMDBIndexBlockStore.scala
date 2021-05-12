package coop.rchain.blockstorage

import java.nio.ByteBuffer
import java.nio.file._
import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.mtl.MonadState
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.StorageError.StorageErr
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{IOError, _}
import coop.rchain.casper.protocol.{
  ApprovedBlock,
  ApprovedBlockProto,
  BlockMessage,
  BlockMessageProto
}
import coop.rchain.lmdb.{Context, LMDBStore}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.ByteStringOps._
import coop.rchain.shared.{AtomicMonadState, Log}
import monix.execution.atomic.AtomicAny
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import fs2._
import monix.eval.Task
import org.lmdbjava._

import scala.util.matching.Regex

private final case class FileLMDBIndexBlockStoreState[F[_]: Sync](
    blockMessageRandomAccessFile: RandomAccessIO[F],
    checkpoints: Map[Int, Checkpoint],
    currentIndex: Int
)

class FileLMDBIndexBlockStore[F[_]: Monad: Sync: RaiseIOError: Log] private (
    lock: Semaphore[F],
    index: LMDBStore[F],
    storagePath: Path,
    approvedBlockPath: Path,
    checkpointsDir: Path,
    state: MonadState[F, FileLMDBIndexBlockStoreState[F]]
) extends BlockStore[F] {
  private case class IndexEntry(checkpointIndex: Int, offset: Long)
  private object IndexEntry {
    def load(byteBuffer: ByteBuffer): IndexEntry = {
      val index  = byteBuffer.getInt()
      val offset = byteBuffer.getLong()
      IndexEntry(index, offset)
    }
  }

  private[this] def getBlockMessageRandomAccessFile: F[RandomAccessIO[F]] =
    state.get.map(_.blockMessageRandomAccessFile)
  private[this] def setBlockMessageRandomAccessFile(file: RandomAccessIO[F]): F[Unit] =
    state.modify(_.copy(blockMessageRandomAccessFile = file))
  private[this] def getCheckpoints: F[Map[Int, Checkpoint]] =
    state.get.map(_.checkpoints)
  private[this] def modifyCheckpoints(f: Map[Int, Checkpoint] => Map[Int, Checkpoint]): F[Unit] =
    state.modify(s => s.copy(checkpoints = f(s.checkpoints)))
  private[this] def getCurrentIndex: F[Int] =
    state.get.map(_.currentIndex)
  private[this] def modifyCurrentIndex(f: Int => Int): F[Unit] =
    state.modify(s => s.copy(currentIndex = f(s.currentIndex)))

  private def readBlockMessage(indexEntry: IndexEntry): F[BlockMessageProto] = {
    def readBlockMessageFromFile(storageFile: RandomAccessIO[F]): F[BlockMessageProto] =
      for {
        _                      <- storageFile.seek(indexEntry.offset)
        blockMessageSizeOpt    <- storageFile.readInt
        blockMessagesByteArray = Array.ofDim[Byte](blockMessageSizeOpt.get)
        _                      <- storageFile.readFully(blockMessagesByteArray)
        blockMessage           = BlockMessageProto.parseFrom(blockMessagesByteArray)
      } yield blockMessage

    for {
      currentIndex <- getCurrentIndex
      blockMessage <- if (currentIndex == indexEntry.checkpointIndex)
                       for {
                         storageFile  <- getBlockMessageRandomAccessFile
                         blockMessage <- readBlockMessageFromFile(storageFile)
                       } yield blockMessage
                     else
                       for {
                         checkpoints <- getCheckpoints
                         result <- checkpoints.get(indexEntry.checkpointIndex) match {
                                    case Some(checkpoint) =>
                                      Sync[F].bracket {
                                        RandomAccessIO.open[F](
                                          checkpoint.storagePath,
                                          RandomAccessIO.Read
                                        )
                                      } { storageFile =>
                                        readBlockMessageFromFile(storageFile)
                                      } { storageFile =>
                                        storageFile.close
                                      }
                                    case None =>
                                      RaiseIOError[F].raise[BlockMessageProto](
                                        UnavailableReferencedCheckpoint(
                                          indexEntry.checkpointIndex
                                        )
                                      )
                                  }
                       } yield result
    } yield blockMessage
  }

  override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    lock.withPermit(
      for {
        indexEntryBytesOpt     <- index.get_WARNING(blockHash.toDirectByteBuffer)
        indexEntryOpt          = indexEntryBytesOpt.map(IndexEntry.load)
        maybeBlockMessageProto <- indexEntryOpt.traverse(readBlockMessage)
      } yield maybeBlockMessageProto >>= (bmp => BlockMessage.from(bmp).toOption)
    )

  override def find(p: BlockHash => Boolean, n: Int): F[Seq[(BlockHash, BlockMessage)]] =
    for {
      filteredIndex <- index.iterate { iterator =>
                        iterator
                          .map(kv => (ByteString.copyFrom(kv.key()), kv.`val`()))
                          .withFilter { case (key, _) => p(key) }
                          .map { case (key, value) => (key, IndexEntry.load(value)) }
                          // Return only the first n result / stop iteration
                          .take(n)
                          .toList
                      }
      result <- lock.withPermit(filteredIndex.flatTraverse {
                 case (blockHash, indexEntry) =>
                   readBlockMessage(indexEntry)
                     .map(block => List(blockHash -> BlockMessage.from(block).right.get)) // TODO FIX-ME
               })
    } yield result

  def iterateStream: F[Stream[F, BlockMessage]] =
    for {
      indexes <- index.iterate { iterator =>
                  Stream.chunk(
                    Chunk.iterable(
                      iterator
                        .map(kv => (ByteString.copyFrom(kv.key()), kv.`val`()))
                        .map {
                          case (key, value) => (key, IndexEntry.load(value))
                        }
                        .toIterable
                    )
                  )
                }
      result = indexes.evalMap {
        case (_, indexEntry) =>
          readBlockMessage(indexEntry).map(b => BlockMessage.from(b).right.get)
      }
    } yield result

  // Default implementation will use `get` to load the whole block so
  //  this is optimization to only look at the index.
  override def contains(blockHash: BlockHash)(implicit ap: Applicative[F]): F[Boolean] =
    index.get(blockHash.toDirectByteBuffer).map(_.nonEmpty)

  override def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    lock.withPermit(
      for {
        randomAccessFile          <- getBlockMessageRandomAccessFile
        currentIndex              <- getCurrentIndex
        endOfFileOffset           <- randomAccessFile.length
        _                         <- randomAccessFile.seek(endOfFileOffset)
        (blockHash, blockMessage) = f
        blockMessageByteArray     = BlockMessage.toProto(blockMessage).toByteArray
        _                         <- randomAccessFile.writeInt(blockMessageByteArray.length)
        _                         <- randomAccessFile.write(blockMessageByteArray)
        _ <- index.put(
              blockHash.toDirectByteBuffer,
              currentIndex.toByteString.concat(endOfFileOffset.toByteString).toDirectByteBuffer
            )
      } yield ()
    )

  def getApprovedBlock: F[Option[ApprovedBlock]] =
    lock.withPermit(
      readAllBytesFromFile(approvedBlockPath).map {
        case bytes if bytes.isEmpty =>
          None
        case bytes =>
          ApprovedBlock.from(ApprovedBlockProto.parseFrom(bytes)).toOption
      }
    )

  def putApprovedBlock(block: ApprovedBlock): F[Unit] =
    lock.withPermit {
      val tmpFile = approvedBlockPath.resolveSibling(approvedBlockPath.getFileName + ".tmp")
      writeToFile(tmpFile, block.toProto.toByteArray) >>
        moveFile(tmpFile, approvedBlockPath, StandardCopyOption.ATOMIC_MOVE).as(())
    }

  override def checkpoint(): F[Unit] =
    lock.withPermit(
      for {
        checkpointIndex              <- getCurrentIndex
        checkpointPath               = checkpointsDir.resolve(checkpointIndex.toString)
        blockMessageRandomAccessFile <- getBlockMessageRandomAccessFile
        _                            <- blockMessageRandomAccessFile.close
        _                            <- moveFile(storagePath, checkpointPath, StandardCopyOption.ATOMIC_MOVE)
        newBlockMessageRandomAccessFile <- RandomAccessIO
                                            .open[F](storagePath, RandomAccessIO.ReadWrite)
        _ <- setBlockMessageRandomAccessFile(newBlockMessageRandomAccessFile)
        _ <- modifyCheckpoints(
              _.updated(checkpointIndex, Checkpoint(checkpointIndex, checkpointPath))
            )
        _ <- modifyCurrentIndex(_ + 1)
      } yield ()
    )

  override def clear(): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- getBlockMessageRandomAccessFile
        _                            <- index.drop
        result                       <- blockMessageRandomAccessFile.setLength(0)
      } yield result
    )

  override def close(): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- getBlockMessageRandomAccessFile
        _                            <- blockMessageRandomAccessFile.close
        _                            <- index.close
      } yield ()
    )
}

object FileLMDBIndexBlockStore {
  implicit private val FileLMDBIndexBlockStoreMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "file-lmdb-index")

  private val checkpointPattern: Regex = "([0-9]+)".r

  final case class Config(
      storagePath: Path,
      indexPath: Path,
      approvedBlockPath: Path,
      checkpointsDirPath: Path,
      mapSize: Long,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  private[blockstorage] final case class CheckpointIndex(
      env: Env[ByteBuffer],
      index: Dbi[ByteBuffer]
  )

  private[blockstorage] final case class Checkpoint(
      index: Int,
      storagePath: Path
  )

  private def loadCheckpoints[F[_]: Sync: Log: RaiseIOError](
      checkpointsDirPath: Path
  ): F[StorageErr[List[Checkpoint]]] =
    for {
      _                   <- makeDirectory(checkpointsDirPath)
      checkpointFilesList <- listRegularFiles(checkpointsDirPath)
      checkpoints <- checkpointFilesList.flatTraverse { filePath =>
                      filePath.getFileName.toString match {
                        case checkpointPattern(index) =>
                          List(Checkpoint(index.toInt, filePath)).pure[F]
                        case other =>
                          Log[F]
                            .warn(s"Ignoring directory '$other': not a valid checkpoint name") >>
                            List.empty[Checkpoint].pure[F]
                      }
                    }
      sortedCheckpoints = checkpoints.sortBy(_.index)
      result = if (sortedCheckpoints.headOption.forall(_.index == 0)) {
        if (sortedCheckpoints.isEmpty ||
            sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
              case (current, next) => current.index + 1 == next.index
            }) {
          sortedCheckpoints.asRight[StorageError]
        } else {
          CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.storagePath))
            .asLeft[List[Checkpoint]]
        }
      } else {
        CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.storagePath))
          .asLeft[List[Checkpoint]]
      }
    } yield result

  def create[F[_]: Concurrent: Sync: Log: Metrics](
      env: Env[ByteBuffer],
      blockStoreDataDir: Path
  ): F[StorageErr[BlockStore[F]]] =
    create(
      env,
      blockStoreDataDir.resolve("storage"),
      blockStoreDataDir.resolve("approved-block"),
      blockStoreDataDir.resolve("checkpoints")
    )

  def create[F[_]: Concurrent: Sync: Log: Metrics](
      blockStoreDataDir: Path
  ): F[BlockStore[F]] = {
    val blockstoreEnv = Context.env(blockStoreDataDir, 1024L * 1024L * 1024L)
    for {
      blockStore <- create(blockstoreEnv, blockStoreDataDir)
                     .map(_.right.get)
    } yield blockStore
  }

  def create[F[_]: Monad: Concurrent: Sync: Log: Metrics](
      env: Env[ByteBuffer],
      storagePath: Path,
      approvedBlockPath: Path,
      checkpointsDirPath: Path
  ): F[StorageErr[BlockStore[F]]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]
    for {
      lock <- MetricsSemaphore.single[F]
      dbi <- Sync[F].delay {
              env.openDbi(s"block_store_index", MDB_CREATE)
            }
      index                        = LMDBStore[F](env, dbi)
      _                            <- createNewFile(approvedBlockPath)
      blockMessageRandomAccessFile <- RandomAccessIO.open(storagePath, RandomAccessIO.ReadWrite)
      sortedCheckpointsEither      <- loadCheckpoints(checkpointsDirPath)
      result = sortedCheckpointsEither match {
        case Right(sortedCheckpoints) =>
          val checkpointsMap = sortedCheckpoints.map(c => c.index -> c).toMap
          val currentIndex   = sortedCheckpoints.lastOption.map(_.index + 1).getOrElse(0)
          val initialState = FileLMDBIndexBlockStoreState[F](
            blockMessageRandomAccessFile,
            checkpointsMap,
            currentIndex
          )
          (new FileLMDBIndexBlockStore[F](
            lock,
            index,
            storagePath,
            approvedBlockPath,
            checkpointsDirPath,
            new AtomicMonadState[F, FileLMDBIndexBlockStoreState[F]](
              AtomicAny(initialState)
            )
          ): BlockStore[F]).asRight[StorageError]
        case Left(e) => e.asLeft[BlockStore[F]]
      }
    } yield result
  }

}
