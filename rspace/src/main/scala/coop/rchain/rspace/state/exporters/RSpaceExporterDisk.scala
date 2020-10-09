package coop.rchain.rspace.state.exporters

import java.nio.file.{Files, Path}

import cats.effect.Sync
import cats.syntax.all._
import cats.{Monad, Parallel}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history.{Store, StoreConfig, StoreInstances}
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.shared.ByteVectorOps.RichByteVector
import scodec.Codec
import scodec.bits.ByteVector

object RSpaceExporterDisk {
  import coop.rchain.rspace.state.syntax._
  import coop.rchain.rspace.util.Lib._

  def writeToDisk[F[_]: Sync: Parallel, C, P, A, K](
      exporter: RSpaceExporter[F],
      root: Blake2b256Hash,
      dirPath: Path,
      chunkSize: Int
  )(implicit codecC: Codec[C], codecP: Codec[P], codecA: Codec[A], codecK: Codec[K]): F[Unit] = {
    type Param = (Seq[(Blake2b256Hash, Option[Byte])], Int)
    def writeChunkRec(historyStore: Store[F], dataStore: Store[F])(
        p: Param
    ): F[Either[Param, Unit]] = {
      val (startPath, chunk) = p
      println(s"PART ${chunk}")
      val skip      = 0
      val exportAll = exporter.getHistoryAndData(startPath, skip, chunkSize, ByteVector(_))
      for {
        inputs                      <- exportAll
        (inputHistory, inputValues) = inputs

        // Get history and data items
        historyItems = inputHistory.items.toVector
        dataItems    = inputValues.items.toVector

        // Validate items
        _ <- time("Validate state items")(
              RSpaceImporter.validateStateItems[F, C, P, A, K](
                historyItems,
                dataItems,
                startPath,
                chunkSize,
                skip,
                k => historyStore.get(Seq(k), ByteVector(_)).map(_.head)
              )
            )

        // Write to restore store
        _ <- Parallel.parProduct(
              time("COMPLETE LMDB HISTORY WRITE")(
                historyStore.put[ByteVector](historyItems, _.toDirectByteBuffer)
              ),
              time("COMPLETE LMDB VALUES WRITE")(
                dataStore.put[ByteVector](dataItems, _.toDirectByteBuffer)
              )
            )

        _ = println(s"LAST PATH: ${inputHistory.lastPath map RSpaceExporter.pathPretty}")

        _ = println("")

        receivedSize = historyItems.size
        isEnd        = receivedSize < chunkSize
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
        mapSize = 10L * 1024 * 1024 * 1024
      )
    )
    Sync[F].delay(Files.createDirectories(path)) >> newStore
  }
}