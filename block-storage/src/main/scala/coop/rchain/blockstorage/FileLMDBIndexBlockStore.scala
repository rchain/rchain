package coop.rchain.blockstorage

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import cats.Monad
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.implicits._
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.FileLMDBIndexBlockStore.Checkpoint
import coop.rchain.blockstorage.errors.{CheckpointsAreNotConsecutive, CheckpointsDoNotStartFromZero}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.Resources.withResource
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.shared.Log
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.Txn.NotReadyException
import org.lmdbjava._

import scala.collection.JavaConverters._
import scala.ref.WeakReference
import scala.util.matching.Regex

class FileLMDBIndexBlockStore[F[_]: Monad: Sync] private (
    lock: Semaphore[F],
    env: Env[ByteBuffer],
    index: Dbi[ByteBuffer],
    blockMessageRandomAccessFileRef: Ref[F, RandomAccessFile],
    checkpointsRef: Ref[F, List[Checkpoint]]
) extends BlockStore[F] {
  implicit class RichBlockHash(byteVector: BlockHash) {

    def toDirectByteBuffer: ByteBuffer = {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(byteVector.size)
      byteVector.copyTo(buffer)
      // TODO: get rid of this:
      buffer.flip()
      buffer
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
      case (txn, ExitCase.Completed) => Sync[F].delay(txn.close())
      case (txn, _) =>
        Sync[F].delay {
          try {
            txn.abort()
          } catch {
            case ex: NotReadyException =>
              ex.printStackTrace()
              TxnOps.manuallyAbortTxn(txn)
            // vide: rchain/rspace/src/main/scala/coop/rchain/rspace/LMDBOps.scala
          }
          txn.close()
        }
    }

  private[this] def withWriteTxn(f: Txn[ByteBuffer] => Unit): F[Unit] =
    withTxn(env.txnWrite())(f)

  private[this] def withReadTxn[R](f: Txn[ByteBuffer] => R): F[R] =
    withTxn(env.txnRead())(f)

  private def readBlockMessage(offset: Long): F[BlockMessage] =
    for {
      blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
      _                            <- Sync[F].delay { blockMessageRandomAccessFile.seek(offset) }
      blockMessageSize             <- Sync[F].delay { blockMessageRandomAccessFile.readInt() }
      blockMessagesByteArray       = Array.ofDim[Byte](blockMessageSize)
      _                            <- Sync[F].delay { blockMessageRandomAccessFile.readFully(blockMessagesByteArray) }
      blockMessage                 = BlockMessage.parseFrom(blockMessagesByteArray)
    } yield blockMessage

  override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    lock.withPermit(
      for {
        indexOpt <- withReadTxn { txn =>
                     Option(index.get(txn, blockHash.toDirectByteBuffer))
                       .map(r => r.getLong)
                   }
        result <- indexOpt.traverse(readBlockMessage)
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
        result <- filteredIndex.traverse {
                   case (blockHash, offset) => readBlockMessage(offset).map(blockHash -> _)
                 }
      } yield result
    )

  override def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    lock.withPermit(
      for {
        blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
        endOfFileOffset              <- Sync[F].delay { blockMessageRandomAccessFile.length() }
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.seek(endOfFileOffset) }
        (blockHash, blockMessage)    = f
        blockMessageByteArray        = blockMessage.toByteArray
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.writeInt(blockMessageByteArray.length) }
        _                            <- Sync[F].delay { blockMessageRandomAccessFile.write(blockMessageByteArray) }
        _ <- withWriteTxn { txn =>
              index.put(
                txn,
                blockHash.toDirectByteBuffer,
                endOfFileOffset.toByteString.toDirectByteBuffer
              )
            }
      } yield ()
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

  private def loadCheckpoints[F[_]: Sync: Log](checkpointsDirPath: Path): F[List[Checkpoint]] =
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
      result <- if (sortedCheckpoints.headOption.forall(_.start == 0)) {
                 if (sortedCheckpoints.isEmpty ||
                     sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
                       case (current, next) => current.end == next.start
                     }) {
                   sortedCheckpoints.pure[F]
                 } else {
                   Sync[F].raiseError(
                     CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.dirPath))
                   )
                 }
               } else {
                 Sync[F].raiseError(CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.dirPath)))
               }
    } yield result

  def create[F[_]: Concurrent: Log](blockStoreDataDir: Path, mapSize: Long): F[BlockStore[F]] =
    create(
      Config(
        blockStoreDataDir.resolve("storage"),
        blockStoreDataDir.resolve("index"),
        blockStoreDataDir.resolve("checkpoints"),
        mapSize
      )
    )

  def create[F[_]: Monad: Concurrent: Log](config: Config): F[BlockStore[F]] =
    for {
      lock <- Semaphore[F](1)
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
      index <- Sync[F].delay { env.openDbi(s"block_store_index", MDB_CREATE) }
      blockMessageRandomAccessFile <- Sync[F].delay {
                                       new RandomAccessFile(config.storagePath.toFile, "rw")
                                     }
      blockMessageRandomAccessFileRef <- Ref.of[F, RandomAccessFile](blockMessageRandomAccessFile)
      sortedCheckpoints               <- loadCheckpoints(config.checkpointsDirPath)
      checkPointsRef                  <- Ref.of[F, List[Checkpoint]](sortedCheckpoints)
    } yield
      new FileLMDBIndexBlockStore[F](
        lock,
        env,
        index,
        blockMessageRandomAccessFileRef,
        checkPointsRef
      )
}
