package coop.rchain.blockstorage

import java.io.{IOException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import cats.Monad
import cats.data.EitherT
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.implicits._
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.StorageError.{StorageErr, StorageIOErr, StorageIOErrT}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.Resources.withResource
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.shared.Log
import coop.rchain.shared.ByteStringOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._

import scala.collection.JavaConverters._
import scala.ref.WeakReference
import scala.util.control.NonFatal
import scala.util.matching.Regex

class FileLMDBIndexBlockStore[F[_]: Monad: Sync: Log] private (
    lock: Semaphore[F],
    env: Env[ByteBuffer],
    index: Dbi[ByteBuffer],
    blockMessageRandomAccessFileRef: Ref[F, RandomAccessFile],
    checkpointsRef: Ref[F, List[Checkpoint]]
) extends BlockStore[F] {
  private[this] def withTxn[R](txnThunk: => Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].bracketCase(Sync[F].delay(txnThunk)) { txn =>
      Sync[F].delay {
        val r = f(txn)
        txn.commit()
        r
      }
    } {
      case (txn, ExitCase.Error(NonFatal(ex))) =>
        Sync[F].delay {
          ex.printStackTrace()
          txn.close()
        } *> Sync[F].raiseError(ex)
      case (txn, _) => Sync[F].delay(txn.close())
    }

  private[this] def withWriteTxn(f: Txn[ByteBuffer] => Unit): F[Unit] =
    withTxn(env.txnWrite())(f)

  private[this] def withReadTxn[R](f: Txn[ByteBuffer] => R): F[R] =
    withTxn(env.txnRead())(f)

  private def readBlockMessage(offset: Long): StorageIOErrT[F, BlockMessage] =
    for {
      randomAccessFile <- EitherT.liftF[F, StorageIOError, RandomAccessFile](
                           blockMessageRandomAccessFileRef.get
                         )
      _ <- EitherT(Sync[F].delay { randomAccessFile.seek(offset) }.attempt)
            .leftMap[StorageIOError] {
              case e: IOException => FileSeekFailed(e)
              case e              => UnexpectedIOStorageError(e)
            }
      blockMessageSize <- EitherT(Sync[F].delay { randomAccessFile.readInt() }.attempt)
                           .leftMap[StorageIOError] {
                             case e: IOException => IntReadFailed(e)
                             case e              => UnexpectedIOStorageError(e)
                           }
      blockMessagesByteArray = Array.ofDim[Byte](blockMessageSize)
      _ <- EitherT(Sync[F].delay { randomAccessFile.readFully(blockMessagesByteArray) }.attempt)
            .leftMap[StorageIOError] {
              case e: IOException => ByteArrayReadFailed(e)
              case e              => UnexpectedIOStorageError(e)
            }
      blockMessage = BlockMessage.parseFrom(blockMessagesByteArray)
    } yield blockMessage

  override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    lock.withPermit(
      for {
        indexOpt <- withReadTxn { txn =>
                     Option(index.get(txn, blockHash.toDirectByteBuffer))
                       .map(r => r.getLong)
                   }
        readResult <- indexOpt.traverse(readBlockMessage).value
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
                              .map { case (key, value) => (key, value.getLong()) }
                              .toList
                          }
                        }
        result <- filteredIndex.flatTraverse {
                   case (blockHash, offset) =>
                     readBlockMessage(offset).value.flatMap {
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
                endOfFileOffset.toByteString.toDirectByteBuffer
              )
            })
      } yield ()).value
    )

  override def checkpoint(): F[Unit] =
    ().pure[F]

  override def clear(): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.setLength(0) }
        _ <- withWriteTxn { txn =>
              index.drop(txn)
            }
      } yield ()
    )

  override def close(): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.close() }
        _                            <- Sync[F].delay { env.close() }
      } yield ()
    )
}

object FileLMDBIndexBlockStore {
  private val checkpointPattern: Regex = "([0-9]+)-([0-9]+)".r

  case class Config(
      storagePath: Path,
      indexPath: Path,
      checkpointsDirPath: Path,
      mapSize: Long,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  private[blockstorage] case class CheckpointIndex(
      env: Env[ByteBuffer],
      index: Dbi[ByteBuffer]
  )

  private[blockstorage] case class Checkpoint(
      start: Long,
      end: Long,
      dirPath: Path,
      storagePath: Path,
      indexPath: Path,
      index: Option[WeakReference[CheckpointIndex]]
  )

  private def loadCheckpoints[F[_]: Sync: Log](
      checkpointsDirPath: Path
  ): F[Either[StorageError, List[Checkpoint]]] =
    for {
      checkpointDirectories <- Sync[F].delay {
                                checkpointsDirPath.toFile.mkdir()
                                Files.list(checkpointsDirPath).filter(p => Files.isDirectory(p))
                              }
      checkpointDirectoriesList = checkpointDirectories
        .collect(Collectors.toList[Path])
        .asScala
        .toList
      checkpoints <- checkpointDirectoriesList.flatTraverse { dirPath =>
                      dirPath.getFileName.toString match {
                        case checkpointPattern(start, end) =>
                          List(
                            Checkpoint(
                              start.toLong,
                              end.toLong,
                              dirPath,
                              dirPath.resolve("storage"),
                              dirPath.resolve("index"),
                              None
                            )
                          ).pure[F]
                        case other =>
                          Log[F].warn(s"Ignoring directory '$other': not a valid checkpoint name") *>
                            List.empty[Checkpoint].pure[F]
                      }
                    }
      sortedCheckpoints = checkpoints.sortBy(_.start)
      result = if (sortedCheckpoints.headOption.forall(_.start == 0)) {
        if (sortedCheckpoints.isEmpty ||
            sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
              case (current, next) => current.end == next.start
            }) {
          sortedCheckpoints.asRight[StorageError]
        } else {
          CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.dirPath)).asLeft[List[Checkpoint]]
        }
      } else {
        CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.dirPath)).asLeft[List[Checkpoint]]
      }
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
    for {
      lock  <- Semaphore[F](1)
      index <- Sync[F].delay { env.openDbi(s"block_store_index", MDB_CREATE) }
      blockMessageRandomAccessFile <- Sync[F].delay {
                                       new RandomAccessFile(storagePath.toFile, "rw")
                                     }
      blockMessageRandomAccessFileRef <- Ref.of[F, RandomAccessFile](blockMessageRandomAccessFile)
      sortedCheckpoints               <- loadCheckpoints(checkpointsDirPath)
      checkpointsRef                  <- sortedCheckpoints.traverse(Ref.of[F, List[Checkpoint]])
      result = checkpointsRef.map { ref =>
        new FileLMDBIndexBlockStore[F](
          lock,
          env,
          index,
          blockMessageRandomAccessFileRef,
          ref
        )
      }
    } yield result

  def create[F[_]: Monad: Concurrent: Log](config: Config): F[StorageErr[BlockStore[F]]] =
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
