package coop.rchain.blockstorage

import java.io.RandomAccessFile
import java.nio.file.Path

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.effect.concurrent.{Ref, Semaphore}
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage

class FileLMDBIndexBlockStore[F[_]: Monad: Sync] private (
  lock: Semaphore[F],
  indexRef: Ref[F, Map[BlockHash, Long]],
  blockMessageRandomAccessFileRef: Ref[F, RandomAccessFile]
) extends BlockStore[F] {
  private def readBlockMessage(offset: Long): F[BlockMessage] =
    for {
      blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
      _ <- Sync[F].delay { blockMessageRandomAccessFile.seek(offset) }
      blockMessageSize <- Sync[F].delay { blockMessageRandomAccessFile.readInt() }
      blockMessagesByteArray = Array.ofDim[Byte](blockMessageSize)
      _ <- Sync[F].delay { blockMessageRandomAccessFile.readFully(blockMessagesByteArray) }
      blockMessage = BlockMessage.parseFrom(blockMessagesByteArray)
    } yield blockMessage
  
  override def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _ <- lock.acquire
      index <- indexRef.get
      result <- index.get(blockHash).traverse(readBlockMessage)
      _ <- lock.release
    } yield result

  override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
    for {
      _ <- lock.acquire
      index <- indexRef.get
      filteredIndex = index.filter { case (blockHash, _) => p(blockHash) }
      result <- filteredIndex.toList.traverse {
        case (blockHash, offset) => readBlockMessage(offset).map(blockHash -> _)
      }
      _ <- lock.release
    } yield result

  override def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- lock.acquire
      blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
      (blockHash, blockMessage) = f
      blockMessageByteArray = blockMessage.toByteArray
      endOfFileOffset <- Sync[F].delay { blockMessageRandomAccessFile.length() }
      _ <- Sync[F].delay { blockMessageRandomAccessFile.seek(endOfFileOffset) }
      _ <- Sync[F].delay { blockMessageRandomAccessFile.writeInt(blockMessageByteArray.length) }
      _ <- Sync[F].delay { blockMessageRandomAccessFile.write(blockMessageByteArray) }
      _ <- indexRef.update(_.updated(blockHash, endOfFileOffset))
      _ <- lock.release
    } yield ()

  override def asMap(): F[Map[BlockHash, BlockMessage]] =
    find(_ => true).map(_.toMap)
  
  override def clear(): F[Unit] =
    for {
      _                            <- lock.acquire
      blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
      _                            <- Sync[F].delay { blockMessageRandomAccessFile.setLength(0) }
      _                            <- indexRef.update(_ => Map.empty)
      _                            <- lock.release
    } yield ()

  override def close(): F[Unit] =
    for {
      _                            <- lock.acquire
      blockMessageRandomAccessFile <- blockMessageRandomAccessFileRef.get
      _                            <- Sync[F].delay { blockMessageRandomAccessFile.close() }
      _                            <- lock.release
    } yield ()
}

object FileLMDBIndexBlockStore {
  case class Config(
    path: Path
  )
  
  def create[F[_]: Monad: Sync: Concurrent](config: Config): F[FileLMDBIndexBlockStore[F]] =
    for {
      lock                  <- Semaphore[F](1)
      blockMessageRandomAccessFile = new RandomAccessFile(config.path.toFile, "rw")
      indexRef <- Ref.of[F, Map[BlockHash, Long]](Map.empty)
      blockMessageRandomAccessFileRef <- Ref.of[F, RandomAccessFile](blockMessageRandomAccessFile)
    } yield new FileLMDBIndexBlockStore[F](lock, indexRef, blockMessageRandomAccessFileRef)
}
