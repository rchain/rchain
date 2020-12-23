package coop.rchain.blockstorage.finality

import java.nio.file.{Path, StandardCopyOption}

import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.LastFinalizedBlockIsCorrupted
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Cell

class LastFinalizedFileStorage[F[_]: Sync: RaiseIOError] private (
    lock: Semaphore[F],
    lastFinalizedPath: Path,
    lastFinalizedBlockHashState: Cell[F, Option[BlockHash]]
) extends LastFinalizedStorage[F] {
  override def put(blockHash: BlockHash): F[Unit] =
    lock.withPermit(
      for {
        _ <- lastFinalizedBlockHashState.set(blockHash.some)

        tmpPath <- createSameDirectoryTemporaryFile(lastFinalizedPath)
        _       <- writeToFile(tmpPath, blockHash.toByteArray)
        _       <- replaceFile(tmpPath, lastFinalizedPath)
      } yield ()
    )

  override def get(): F[Option[BlockHash]] =
    lock.withPermit(
      lastFinalizedBlockHashState.read
    )
}

object LastFinalizedFileStorage {
  private def readLastFinalizedBlockHash[F[_]: Sync: RaiseIOError](
      lastFinalizedPath: Path
  ): F[Option[BlockHash]] =
    createNewFile[F](lastFinalizedPath) >>
      readAllBytesFromFile(lastFinalizedPath).flatMap {
        case bytes if bytes.isEmpty =>
          none[BlockHash].pure[F]
        case bytes if bytes.length == BlockHash.Length =>
          ByteString.copyFrom(bytes).some.pure[F]
        case _ =>
          Sync[F].raiseError[Option[BlockHash]](LastFinalizedBlockIsCorrupted)
      }

  def make[F[_]: Sync: Concurrent](
      lastFinalizedPath: Path
  ): F[LastFinalizedStorage[F]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]

    for {
      lock                      <- Semaphore[F](1)
      lastFinalizedBlockHashOpt <- readLastFinalizedBlockHash(lastFinalizedPath)
      state                     <- Cell.mvarCell[F, Option[BlockHash]](lastFinalizedBlockHashOpt)
    } yield new LastFinalizedFileStorage[F](lock, lastFinalizedPath, state)
  }
}
