package coop.rchain.blockstorage

import java.io._
import java.nio.ByteBuffer
import java.nio.file._

import cats.Monad
import cats.data.EitherT
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.implicits._
import cats.effect.concurrent.Semaphore
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.StorageError.{StorageErr, StorageErrT, StorageIOErr, StorageIOErrT}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.Resources.withResource
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io._
import coop.rchain.blockstorage.StorageError._
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

class FileLMDBIndexBlockStore[F[_]: Monad: Sync: Log] private (
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

  private def readBlockMessage(indexEntry: IndexEntry): StorageIOErrT[F, BlockMessage] = {
    def readBlockMessageFromFile(storageFile: RandomAccessIO[F]): StorageIOErrT[F, BlockMessage] =
      for {
        _                      <- EitherT(storageFile.seek(indexEntry.offset)).toStorageIOErrT
        blockMessageSize       <- EitherT(storageFile.readInt).toStorageIOErrT
        blockMessagesByteArray = Array.ofDim[Byte](blockMessageSize)
        _                      <- EitherT(storageFile.readFully(blockMessagesByteArray)).toStorageIOErrT
        blockMessage           = BlockMessage.parseFrom(blockMessagesByteArray)
      } yield blockMessage

    for {
      currentIndex <- EitherT.liftF(getCurrentIndex)
      blockMessage <- if (currentIndex == indexEntry.checkpointIndex)
                       for {
                         storageFile  <- EitherT.liftF(getBlockMessageRandomAccessFile)
                         blockMessage <- readBlockMessageFromFile(storageFile)
                       } yield blockMessage
                     else
                       for {
                         checkpoints <- EitherT.liftF(getCheckpoints)
                         result <- checkpoints.get(indexEntry.checkpointIndex) match {
                                    case Some(checkpoint) =>
                                      type StorageIOErrTF[A] = StorageIOErrT[F, A]
                                      Sync[StorageIOErrTF].bracket {
                                        RandomAccessIO.open[F](checkpoint.storagePath)
                                      } { storageFile =>
                                        readBlockMessageFromFile(storageFile)
                                      } { storageFile =>
                                        EitherT(storageFile.close()).toStorageIOErrT
                                      }
                                    case None =>
                                      EitherT
                                        .leftT[F, BlockMessage]
                                        .apply[StorageIOError](
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
        readResult <- indexEntryOpt.traverse(readBlockMessage).value
        result <- readResult match {
                   case Left(storageReadError) =>
                     Log[F].error(storageReadError.message) *> none[BlockMessage].pure[F]
                   case Right(block) => block.pure[F]
                 }
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
                     readBlockMessage(indexEntry).value.flatMap {
                       case Left(storageReadError) =>
                         Log[F].error(storageReadError.message) *> List
                           .empty[(BlockHash, BlockMessage)]
                           .pure[F]
                       case Right(block) =>
                         List(blockHash -> block).pure[F]
                     }
                 }
      } yield result
    )

  override def put(f: => (BlockHash, BlockMessage)): F[StorageIOErr[Unit]] =
    lock.withPermit(
      (for {
        randomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessIO[F]](
                             getBlockMessageRandomAccessFile
                           )
        currentIndex              <- EitherT.liftF(getCurrentIndex)
        endOfFileOffset           <- EitherT(randomAccessFile.length).toStorageIOErrT
        _                         <- EitherT(randomAccessFile.seek(endOfFileOffset)).toStorageIOErrT
        (blockHash, blockMessage) = f
        blockMessageByteArray     = blockMessage.toByteArray
        _                         <- EitherT(randomAccessFile.writeInt(blockMessageByteArray.length)).toStorageIOErrT
        _                         <- EitherT(randomAccessFile.write(blockMessageByteArray)).toStorageIOErrT
        _ <- EitherT.liftF[F, StorageIOError, Unit](withWriteTxn { txn =>
              index.put(
                txn,
                blockHash.toDirectByteBuffer,
                currentIndex.toByteString.concat(endOfFileOffset.toByteString).toDirectByteBuffer
              )
            })
      } yield ()).value
    )

  override def checkpoint(): F[StorageIOErr[Unit]] =
    lock.withPermit(
      (for {
        checkpointIndex <- EitherT.liftF[F, StorageIOError, Int](getCurrentIndex)
        checkpointPath  = checkpointsDir.resolve(checkpointIndex.toString)
        blockMessageRandomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessIO[F]](
                                         getBlockMessageRandomAccessFile
                                       )
        _                               <- EitherT(blockMessageRandomAccessFile.close()).toStorageIOErrT
        _                               <- moveFile(storagePath, checkpointPath, StandardCopyOption.ATOMIC_MOVE).toStorageIOErrT
        newBlockMessageRandomAccessFile <- RandomAccessIO.open[F](storagePath).toStorageIOErrT
        _ <- EitherT.liftF[F, StorageIOError, Unit](
              setBlockMessageRandomAccessFile(newBlockMessageRandomAccessFile)
            )
        _ <- EitherT.liftF[F, StorageIOError, Unit](
              modifyCheckpoints(
                _.updated(checkpointIndex, Checkpoint(checkpointIndex, checkpointPath))
              )
            )
        _ <- EitherT.liftF[F, StorageIOError, Unit](modifyCurrentIndex(_ + 1))
      } yield ()).value
    )

  override def clear(): F[StorageIOErr[Unit]] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- getBlockMessageRandomAccessFile
        _ <- withWriteTxn { txn =>
              index.drop(txn)
            }
        result <- EitherT(blockMessageRandomAccessFile.setLength(0)).toStorageIOErrT.value
      } yield result
    )

  override def close(): F[StorageIOErr[Unit]] =
    lock.withPermit(
      (for {
        blockMessageRandomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessIO[F]](
                                         getBlockMessageRandomAccessFile
                                       )
        _ <- EitherT(blockMessageRandomAccessFile.close()).toStorageIOErrT
        _ <- EitherT(Sync[F].delay { env.close() }.attempt).leftMap[StorageIOError] {
              case e: IOException =>
                WrappedIOError(ClosingFailed(e))
              case t =>
                WrappedIOError(UnexpectedIOError(t))
            }
      } yield ()).value
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

  private def loadCheckpoints[F[_]: Sync: Log](
      checkpointsDirPath: Path
  ): StorageErrT[F, List[Checkpoint]] =
    for {
      checkpointFilesList <- listFiles(checkpointsDirPath).toStorageIOErrT
      checkpoints <- EitherT.liftF[F, StorageError, List[Checkpoint]](
                      checkpointFilesList.flatTraverse { filePath =>
                        filePath.getFileName.toString match {
                          case checkpointPattern(index) =>
                            List(Checkpoint(index.toInt, filePath)).pure[F]
                          case other =>
                            Log[F]
                              .warn(s"Ignoring directory '$other': not a valid checkpoint name") *>
                              List.empty[Checkpoint].pure[F]
                        }
                      }
                    )
      sortedCheckpoints = checkpoints.sortBy(_.index)
      result <- EitherT.fromEither[F](if (sortedCheckpoints.headOption.forall(_.index == 0)) {
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
               })
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
  ): F[StorageErr[BlockStore[F]]] =
    (for {
      lock <- EitherT.liftF[F, StorageError, Semaphore[F]](Semaphore[F](1))
      index <- EitherT.liftF[F, StorageError, Dbi[ByteBuffer]](Sync[F].delay {
                env.openDbi(s"block_store_index", MDB_CREATE)
              })
      blockMessageRandomAccessFile <- RandomAccessIO.open(storagePath).toStorageIOErrT
      sortedCheckpoints            <- loadCheckpoints(checkpointsDirPath)
      checkpointsMap               = sortedCheckpoints.map(c => c.index -> c).toMap
      currentIndex                 = sortedCheckpoints.lastOption.map(_.index + 1).getOrElse(0)
      initialState = FileLMDBIndexBlockStoreState[F](
        blockMessageRandomAccessFile,
        checkpointsMap,
        currentIndex
      )
      result = new FileLMDBIndexBlockStore[F](
        lock,
        env,
        index,
        storagePath,
        checkpointsDirPath,
        new AtomicMonadState[F, FileLMDBIndexBlockStoreState[F]](AtomicAny(initialState))
      )
    } yield result: BlockStore[F]).value

  def create[F[_]: Monad: Concurrent: Capture: Log](
      config: Config
  ): F[StorageErr[BlockStore[F]]] =
    for {
      env <- Sync[F].delay {
              if (Files.notExists(config.indexPath)) Files.createDirectories(config.indexPath)
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
