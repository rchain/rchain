package coop.rchain.rspace.state.exporters

import cats.Monad
import cats.effect.Async
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.shared.{Log, Serialize, Stopwatch}
import coop.rchain.store.{KeyValueStore, LmdbStoreManager}
import fs2.Stream
import scodec.bits.ByteVector

import java.nio.file.Path

object RSpaceExporterDisk {

  def writeToDisk[F[_]: Async: Log](
      exporter: RSpaceExporter[F],
      root: Blake2b256Hash,
      dirPath: Path,
      chunkSize: Int
  ): F[Unit] = {
    type Param = (Seq[(Blake2b256Hash, Option[Byte])], Int)
    def writeChunkRec(historyStore: KeyValueStore[F], dataStore: KeyValueStore[F])(
        p: Param
    ): F[Either[Param, Unit]] = {
      val (startPath, chunk) = p
      val skip               = 0
      val exportAll          = exporter.getHistoryAndData(startPath, skip, chunkSize, ByteVector(_))
      for {
        inputs                      <- exportAll
        (inputHistory, inputValues) = inputs

        // Get history and data items
        historyItems = inputHistory.items.toVector
        dataItems    = inputValues.items.toVector

        // Validate items
        validationProcess = Stopwatch.time(Log[F].info(_))("Validate state items")(
          RSpaceImporter.validateStateItems[F](
            historyItems,
            dataItems,
            startPath,
            chunkSize,
            skip,
            k => historyStore.get(Seq(k.bytes.toDirectByteBuffer), ByteVector(_)).map(_.head)
          )
        )

        // Restore operations / run in parallel
        _ <- Stream(
              validationProcess,
              Stopwatch.time(Log[F].info(_))("Write history items")(
                historyStore.put[ByteVector](historyItems.map {
                  case (b, v) => (b.bytes.toDirectByteBuffer, v)
                }, _.toDirectByteBuffer)
              ),
              Stopwatch.time(Log[F].info(_))("Write data items")(
                dataStore.put[ByteVector](
                  dataItems.map { case (b, v) => (b.bytes.toDirectByteBuffer, v) },
                  _.toDirectByteBuffer
                )
              )
            ).map(Stream.eval).parJoinUnbounded.compile.drain

        _ = Log[F].info(s"Last path: ${inputHistory.lastPath map RSpaceExporter.pathPretty}")

        receivedSize = historyItems.size
        isEnd        = receivedSize < chunkSize
      } yield if (isEnd) ().asRight else (inputHistory.lastPath, chunk + 1).asLeft
    }

    for {
      // Lmdb restore history
      lmdbHistoryManager <- LmdbStoreManager(dirPath.resolve("history"), 10L * 1024 * 1024 * 1024)
      lmdbHistoryStore   <- lmdbHistoryManager.store("db")
      lmdbDataManager    <- LmdbStoreManager(dirPath.resolve("cold"), 10L * 1024 * 1024 * 1024)
      lmdbDataStore      <- lmdbDataManager.store("db")
      _ <- Stopwatch.time(Log[F].info(_))("Restore complete")(
            Monad[F]
              .tailRecM((Seq((root, none[Byte])), 0))(
                writeChunkRec(lmdbHistoryStore, lmdbDataStore)
              )
          )
    } yield ()
  }
}
