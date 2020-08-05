package coop.rchain.rspace.state.exporters

import java.nio.file.{Files, Path}

import cats.effect.Sync
import cats.syntax.all._
import cats.{Monad, Parallel}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history.{Store, StoreConfig, StoreInstances}
import coop.rchain.rspace.state.RSpaceExporter
import scodec.bits.BitVector

object RSpaceExporterDisk {
  import coop.rchain.rspace.state.syntax._
  import coop.rchain.rspace.util.Lib._

  def writeToDisk[F[_]: Sync: Parallel](
      exporter: RSpaceExporter[F],
      root: Blake2b256Hash,
      dirPath: Path,
      chunkSize: Int
  ): F[Unit] = {
    type Param = (Seq[(Blake2b256Hash, Option[Byte])], Int)
    def writeChunkRec(historyStore: Store[F], dataStore: Store[F])(
        p: Param
    ): F[Either[Param, Unit]] = {
      val (startPath, chunk) = p
      println(s"PART ${chunk}")
      val skip          = 0 // if (chunk == 0) chunkSize * 7 - 7 else 0
      val exportHistory = exporter.getHistory(startPath, skip, chunkSize, BitVector(_))
      val exportData    = exporter.getData(startPath, skip, chunkSize, BitVector(_))
      for {
//        inputHistory <- exportHistory
//        inputValues  <- exportData
        inputs                      <- Parallel.parProduct(exportHistory, exportData)
        (inputHistory, inputValues) = inputs

        // Get history
        partialStoreList = inputHistory.items

        // Get values
        partialValuesList = inputValues.items

        // Restore lmdb stores
        historyStore <- mkLmdbInstance(dirPath.resolve("history"))
        dataStore    <- mkLmdbInstance(dirPath.resolve("cold"))

        // Write to restore store
//        _ <- time("COMPLETE LMDB HISTORY WRITE")(historyStore.put(partialStoreList))
//        _ <- time("COMPLETE LMDB VALUES WRITE")(dataStore.put(partialValuesList))
        _ <- Parallel.parProduct(
              time("COMPLETE LMDB HISTORY WRITE")(historyStore.put(partialStoreList)),
              time("COMPLETE LMDB VALUES WRITE")(dataStore.put(partialValuesList))
            )

        _ <- historyStore.close()
        _ <- dataStore.close()

        _ = println(s"LAST PATH: ${inputHistory.lastPath.map { case (x, y) => (x, toHex(y)) }}")

        _ = println("")

        isEnd = partialStoreList.size < chunkSize
      } yield if (isEnd) ().asRight else (inputHistory.lastPath, chunk + 1).asLeft
    }

    for {
      // Lmdb restore history
      lmdbHistoryStore <- mkLmdbInstance(dirPath.resolve("history"))
      lmdbDataStore    <- mkLmdbInstance(dirPath.resolve("cold"))
      _ <- time("RESTORE COMPLETE")(
            Monad[F]
              .tailRecM((Seq((root, none[Byte])), 0))(
                writeChunkRec(lmdbHistoryStore, lmdbDataStore)
              )
          )
      _ <- lmdbHistoryStore.close()
      _ <- lmdbDataStore.close()
    } yield ()
  }

  def mkLmdbInstance[F[_]: Sync](path: Path): F[Store[F]] = {
    val newStore = StoreInstances.lmdbStore[F](
      StoreConfig(
        path = path,
        mapSize = 10073741824L
      )
    )
    Sync[F].delay(Files.createDirectories(path)) >> newStore
  }
}
