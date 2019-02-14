package coop.rchain.blockstorage

import java.io._
import java.nio.ByteBuffer
import java.nio.file._

import cats.{Functor, Monad}
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.implicits._
import cats.effect.concurrent.Semaphore
import cats.mtl.{FunctorRaise, MonadState}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.StorageError.StorageErr
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.Resources.withResource
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.catscontrib.Capture
import coop.rchain.shared.{AtomicMonadState, Log}
import coop.rchain.shared.ByteStringOps._
import monix.execution.atomic.AtomicAny
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.matching.Regex

private final case class FileLMDBIndexBlockStoreState[F[_]: Sync](
    blockMessageRandomAccessFile: RandomAccessIO[F],
    checkpoints: Map[Int, Checkpoint],
    currentIndex: Int
)

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class FileLMDBIndexBlockStore[F[_]: Monad: Sync: RaiseIOError: Log] private (
    lock: Semaphore[F],
    env: Env[ByteBuffer],
    index: Dbi[ByteBuffer],
    storagePath: Path,
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

  private[this] def withTxn[R](txnThunk: => Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].bracketCase(Sync[F].delay(txnThunk)) { txn =>
      Sync[F].delay {
        val r = f(txn)
        txn.commit()
        r
      }
    } {
      case (txn, ExitCase.Error(NonFatal(ex))) =>
        val stringWriter = new StringWriter()
        ex.printStackTrace(new PrintWriter(stringWriter))
        Log[F].error(stringWriter.toString) *> Sync[F].delay(txn.close()) *> Sync[F].raiseError(ex)
      case (txn, _) => Sync[F].delay(txn.close())
    }

  private[this] def withWriteTxn(f: Txn[ByteBuffer] => Unit): F[Unit] =
    withTxn(env.txnWrite())(f)

  private[this] def withReadTxn[R](f: Txn[ByteBuffer] => R): F[R] =
    withTxn(env.txnRead())(f)

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

  private def readBlockMessage(indexEntry: IndexEntry): F[BlockMessage] = {
    def readBlockMessageFromFile(storageFile: RandomAccessIO[F]): F[BlockMessage] =
      for {
        _                      <- storageFile.seek(indexEntry.offset)
        blockMessageSize       <- storageFile.readInt
        blockMessagesByteArray = Array.ofDim[Byte](blockMessageSize)
        _                      <- storageFile.readFully(blockMessagesByteArray)
        blockMessage           = BlockMessage.parseFrom(blockMessagesByteArray)
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
                                      RaiseIOError[F].raise[BlockMessage](
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
        indexEntryOpt <- withReadTxn { txn =>
                          Option(index.get(txn, blockHash.toDirectByteBuffer))
                            .map(IndexEntry.load)
                        }
        result <- indexEntryOpt.traverse(readBlockMessage)
      } yield result
    )

  override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
    lock.withPermit(
      for {
        filteredIndex <- withReadTxn { txn =>
                          withResource(index.iterate(txn)) { iterator =>
                            iterator.asScala
                              .map(kv => (ByteString.copyFrom(kv.key()), kv.`val`()))
                              .withFilter { case (key, _) => p(key) }
                              .map { case (key, value) => (key, IndexEntry.load(value)) }
                              .toList
                          }
                        }
        result <- filteredIndex.flatTraverse {
                   case (blockHash, indexEntry) =>
                     readBlockMessage(indexEntry)
                       .map(block => List(blockHash -> block))
                 }
      } yield result
    )

  override def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    lock.withPermit(
      for {
        randomAccessFile          <- getBlockMessageRandomAccessFile
        currentIndex              <- getCurrentIndex
        endOfFileOffset           <- randomAccessFile.length
        _                         <- randomAccessFile.seek(endOfFileOffset)
        (blockHash, blockMessage) = f
        blockMessageByteArray     = blockMessage.toByteArray
        _                         <- randomAccessFile.writeInt(blockMessageByteArray.length)
        _                         <- randomAccessFile.write(blockMessageByteArray)
        _ <- withWriteTxn { txn =>
              index.put(
                txn,
                blockHash.toDirectByteBuffer,
                currentIndex.toByteString.concat(endOfFileOffset.toByteString).toDirectByteBuffer
              )
            }
      } yield ()
    )

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
        _ <- withWriteTxn { txn =>
              index.drop(txn)
            }
        result <- blockMessageRandomAccessFile.setLength(0)
      } yield result
    )

  override def close(): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- getBlockMessageRandomAccessFile
        _                            <- blockMessageRandomAccessFile.close
        envCloseResult               <- Sync[F].delay[Unit] { env.close() }.attempt
        _ <- envCloseResult match {
              case Right(_) => ().pure[F]
              case Left(e: IOException) =>
                RaiseIOError[F].raise[Unit](ClosingFailed(e))
              case Left(t) =>
                RaiseIOError[F].raise[Unit](UnexpectedIOError(t))
            }
      } yield ()
    )
}

object FileLMDBIndexBlockStore {
  private val checkpointPattern: Regex = "([0-9]+)".r

  final case class Config(
      storagePath: Path,
      indexPath: Path,
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
                            .warn(s"Ignoring directory '$other': not a valid checkpoint name") *>
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

  def create[F[_]: Concurrent: Capture: Log](
      env: Env[ByteBuffer],
      blockStoreDataDir: Path
  ): F[StorageErr[BlockStore[F]]] =
    create(env, blockStoreDataDir.resolve("storage"), blockStoreDataDir.resolve("checkpoints"))

  def create[F[_]: Monad: Concurrent: Capture: Log](
      env: Env[ByteBuffer],
      storagePath: Path,
      checkpointsDirPath: Path
  ): F[StorageErr[BlockStore[F]]] = {
    implicit val raiseIOErrorThroughSync = new FunctorRaise[F, IOError] {
      override val functor: Functor[F] =
        Functor[F]

      override def raise[A](e: IOError): F[A] =
        Sync[F].raiseError(IOError.ExceptionWrapper(e))
    }
    for {
      lock <- Semaphore[F](1)
      index <- Sync[F].delay {
                env.openDbi(s"block_store_index", MDB_CREATE)
              }
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
            env,
            index,
            storagePath,
            checkpointsDirPath,
            new AtomicMonadState[F, FileLMDBIndexBlockStoreState[F]](
              AtomicAny(initialState)
            )
          ): BlockStore[F]).asRight[StorageError]
        case Left(e) => e.asLeft[BlockStore[F]]
      }
    } yield result
  }

  def create[F[_]: Monad: Concurrent: Capture: Log](
      config: Config
  ): F[StorageErr[BlockStore[F]]] =
    for {
      notExists <- Sync[F].delay(Files.notExists(config.indexPath))
      _         <- if (notExists) Sync[F].delay(Files.createDirectories(config.indexPath)) else ().pure[F]
      env <- Sync[F].delay {
              val flags = if (config.noTls) List(EnvFlags.MDB_NOTLS) else List.empty
              Env
                .create()
                .setMapSize(config.mapSize)
                .setMaxDbs(config.maxDbs)
                .setMaxReaders(config.maxReaders)
                .open(config.indexPath.toFile, flags: _*)
            }
      result <- create[F](env, config.storagePath, config.checkpointsDirPath)
    } yield result
}
