package coop.rchain.blockstorage

import java.io._
import java.nio.ByteBuffer
import java.nio.file.{Files, NotDirectoryException, Path}
import java.util.stream.Collectors

import cats.Monad
import cats.data.EitherT
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.implicits._
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.StorageError.{StorageErr, StorageErrT, StorageIOErr, StorageIOErrT}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.Resources.withResource
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.shared.Log
import coop.rchain.shared.ByteStringOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.util.matching.Regex

class FileLMDBIndexBlockStore[F[_]: Monad: Sync: Log] private (
    lock: Semaphore[F],
    env: Env[ByteBuffer],
    index: Dbi[ByteBuffer],
    storagePath: Path,
    checkpointsDir: Path,
    blockMessageRandomAccessFileRef: Ref[F, RandomAccessFile],
    checkpointsRef: Ref[F, Map[Int, Checkpoint]],
    currentIndexRef: Ref[F, Int]
) extends BlockStore[F] {
  private case class IndexEntry(checkpointIndex: Int, offset: Long)
  private object IndexEntry {
    def load(byteBuffer: ByteBuffer): IndexEntry =
      IndexEntry(byteBuffer.getInt, byteBuffer.getLong)
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

  private def readBlockMessage(offset: Long): StorageIOErrT[F, BlockMessage] = {
    def readBlockMessageFromFile(storageFile: RandomAccessFile): F[BlockMessage] =
      for {
        _ <- EitherT(Sync[F].delay {storageFile.seek(offset)}.attempt)
          .leftMap[StorageIOError] {
          case e: IOException => FileSeekFailed(e)
          case e => UnexpectedIOStorageError(e)
        }
        blockMessageSize <- EitherT(Sync[F].delay {storageFile.readInt()}.attempt)
          .leftMap[StorageIOError] {
          case e: IOException => IntReadFailed(e)
          case e => UnexpectedIOStorageError(e)
        }
        blockMessagesByteArray = Array.ofDim[Byte](blockMessageSize)
        _ <- EitherT(Sync[F].delay {storageFile.readFully(blockMessagesByteArray)}.attempt)
          .leftMap[StorageIOError] {
          case e: IOException => ByteArrayReadFailed(e)
          case e => UnexpectedIOStorageError(e)
        }
        blockMessage = BlockMessage.parseFrom(blockMessagesByteArray)
      } yield blockMessage
    
    for {
      currentIndex <- currentIndexRef.get
      blockMessage <- if (currentIndex == indexEntry.checkpointIndex)
        for {
          storageFile  <- blockMessageRandomAccessFileRef.get
          blockMessage <- readBlockMessageFromFile(storageFile)
        } yield blockMessage
      else
        for {
          checkpoints <- checkpointsRef.get
          result <- checkpoints.get(currentIndex) match {
            case Some(checkpoint) =>
              Sync[F].bracket {
                Sync[F].delay {
                  new RandomAccessFile(checkpoint.storagePath.toFile, "rw")
                }
              } { storageFile =>
                readBlockMessageFromFile(storageFile)
              } { storageFile =>
                Sync[F].delay { storageFile.close() }
              }
            case None =>
              Sync[F].raiseError[BlockMessage](
                UnavailableReferencedCheckpoint(indexEntry.checkpointIndex)
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
        randomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessFile](
                             blockMessageRandomAccessFileRef.get
                           )
        currentIndex                 <- currentIndexRef.get
        endOfFileOffset <- EitherT(Sync[F].delay { randomAccessFile.length() }.attempt)
                            .leftMap[StorageIOError](UnexpectedIOStorageError.apply)
        _ <- EitherT(Sync[F].delay { randomAccessFile.seek(endOfFileOffset) }.attempt)
              .leftMap[StorageIOError] {
                case e: IOException => FileSeekFailed(e)
                case e              => UnexpectedIOStorageError(e)
              }
        (blockHash, blockMessage) = f
        blockMessageByteArray     = blockMessage.toByteArray
        _ <- EitherT(
              Sync[F].delay { randomAccessFile.writeInt(blockMessageByteArray.length) }.attempt
            ).leftMap[StorageIOError] {
              case e: IOException => IntWriteFailed(e)
              case e              => UnexpectedIOStorageError(e)
            }
        _ <- EitherT(Sync[F].delay { randomAccessFile.write(blockMessageByteArray) }.attempt)
              .leftMap[StorageIOError] {
                case e: IOException => ByteArrayWriteFailed(e)
                case e              => UnexpectedIOStorageError(e)
              }
        _ <- EitherT.liftF[F, StorageIOError, Unit](withWriteTxn { txn =>
              index.put(
                txn,
                blockHash.toDirectByteBuffer,
                currentIndex.toByteString.concat(endOfFileOffset.toByteString).toDirectByteBuffer
              )
            })
      } yield ()).value
    )

  override def checkpoint(): F[Unit] =
    lock.withPermit(
      for {
        checkpointIndex              <- currentIndexRef.get
        checkpointPath               = checkpointsDir.resolve(checkpointIndex.toString)
        blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.close() }
        _                            <- Sync[F].delay { Files.move(storagePath, checkpointPath) }
        newBlockMessageRandomAccessFile <- Sync[F].delay {
                                            new RandomAccessFile(storagePath.toFile, "rw")
                                          }
        _ <- blockMessageRandomAccessFileRef.update(_ => newBlockMessageRandomAccessFile)
        _ <- checkpointsRef.update(
              _.updated(checkpointIndex, Checkpoint(checkpointIndex, checkpointPath))
            )
        _ <- currentIndexRef.update(_ + 1)
      } yield ()
    )

  override def clear(): F[StorageIOErr[Unit]] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
        _ <- withWriteTxn { txn =>
              index.drop(txn)
            }
        storageFileClearAttempt <- Sync[F].delay { blockMessageRandomAccessFile.setLength(0) }.attempt
        result = storageFileClearAttempt match {
          case Left(e: IOException) =>
            Left(ClearFileFailed(e))
          case Left(t) =>
            Left(UnexpectedIOStorageError(t))
          case Right(_) =>
            Right(())
        }
      } yield result
    )

  override def close(): F[StorageIOErr[Unit]] =
    lock.withPermit(
      (for {
        blockMessageRandomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessFile](
                                         blockMessageRandomAccessFileRef.get
                                       )
        _ <- EitherT(Sync[F].delay { blockMessageRandomAccessFile.close() }.attempt)
              .leftMap[StorageIOError] {
                case e: IOException =>
                  ClosingFailed(e)
                case t =>
                  UnexpectedIOStorageError(t)
              }
        _ <- EitherT(Sync[F].delay { env.close() }.attempt).leftMap[StorageIOError] {
              case e: IOException =>
                ClosingFailed(e)
              case t =>
                UnexpectedIOStorageError(t)
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

  private def openRandomAccessFile[F[_]: Sync](path: Path): StorageErrT[F, RandomAccessFile] =
    EitherT(Sync[F].delay { new RandomAccessFile(path.toFile, "rw") }.attempt).leftMap {
      case e: FileNotFoundException => FileNotFound(e)
      case e: SecurityException     => FileSecurityViolation(e)
      case t                        => UnexpectedIOStorageError(t)
    }

  private def isDirectory[F[_]: Sync](path: Path): StorageErrT[F, Boolean] =
    EitherT(Sync[F].delay { Files.isDirectory(path) }.attempt).leftMap[StorageError] {
      case e: SecurityException => FileSecurityViolation(e)
      case t                    => UnexpectedIOStorageError(t)
    }

  private def listInDirectory[F[_]: Sync](dirPath: Path): StorageErrT[F, List[Path]] =
    for {
      _ <- EitherT(Sync[F].delay { dirPath.toFile.mkdir() }.attempt).leftMap[StorageError] {
            case e: SecurityException => FileSecurityViolation(e)
            case t                    => UnexpectedIOStorageError(t)
          }
      files <- EitherT(Sync[F].delay { Files.list(dirPath) }.attempt).leftMap[StorageError] {
                      case e: NotDirectoryException => FileIsNotDirectory(e)
                      case e: SecurityException     => FileSecurityViolation(e)
                      case t                        => UnexpectedIOStorageError(t)
                    }
      filesList = files
        .collect(Collectors.toList[Path])
        .asScala
        .toList
    } yield filesList

  private def listDirectories[F[_]: Sync](dirPath: Path): StorageErrT[F, List[Path]] = {
    type TEMP[A] = StorageErrT[F, A]
    for {
      inDirectoryList <- listInDirectory(dirPath)
      directoryList   <- inDirectoryList.filterA[TEMP](isDirectory)
    } yield directoryList
  }

  private def loadCheckpoints[F[_]: Sync: Log](
      checkpointsDirPath: Path
  ): StorageErrT[F, List[Checkpoint]] =
    for {
      checkpointDirectoriesList <- listDirectories(checkpointsDirPath)
      checkpoints <- EitherT.liftF[F, StorageError, List[Checkpoint]](
                      checkpointDirectoriesList.flatTraverse { filePath =>
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

  def create[F[_]: Concurrent: Log](
      env: Env[ByteBuffer],
      blockStoreDataDir: Path
  ): F[StorageErr[BlockStore[F]]] =
    create(env, blockStoreDataDir.resolve("storage"), blockStoreDataDir.resolve("checkpoints"))

  def create[F[_]: Monad: Concurrent: Log](
      env: Env[ByteBuffer],
      storagePath: Path,
      checkpointsDirPath: Path
  ): F[StorageErr[BlockStore[F]]] =
    (for {
      lock <- EitherT.liftF[F, StorageError, Semaphore[F]](Semaphore[F](1))
      index <- EitherT.liftF[F, StorageError, Dbi[ByteBuffer]](Sync[F].delay {
                env.openDbi(s"block_store_index", MDB_CREATE)
              })
      blockMessageRandomAccessFile <- openRandomAccessFile[F](storagePath)
      blockMessageRandomAccessFileRef <- EitherT.liftF[F, StorageError, Ref[F, RandomAccessFile]](
                                          Ref.of[F, RandomAccessFile](blockMessageRandomAccessFile)
                                        )
      sortedCheckpoints <- loadCheckpoints(checkpointsDirPath)
      checkpointsRef <- EitherT.liftF[F, StorageError, Ref[F, List[Checkpoint]]](
                         Ref.of[F, List[Checkpoint]](sortedCheckpoints)
                       )
      currentIndex    = sortedCheckpoints.lastOption.map(_.index + 1).getOrElse(0)
      currentIndexRef <- Ref.of[F, Int](currentIndex)
      result = new FileLMDBIndexBlockStore[F](
        lock,
        env,
        index,
        storagePath,
        checkpointsDirPath,
        blockMessageRandomAccessFileRef,
        checkpointsRef,
        currentIndexRef
      )
    } yield result: BlockStore[F]).value

  def create[F[_]: Monad: Concurrent: Log](
      env: Env[ByteBuffer],
      storagePath: Path,
      checkpointsDirPath: Path
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
