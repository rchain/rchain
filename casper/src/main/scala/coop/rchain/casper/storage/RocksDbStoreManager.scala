package coop.rchain.casper.storage

import java.nio.file.{Files, Path}

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}
import org.rocksdb.util.SizeUnit
import org.rocksdb.{CompactionStyle, CompressionType, Options, RocksDB}

object RocksDbStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    Sync[F].delay(new RocksDbStoreManagerImpl(dirPath))
}

/**
  * Wrapper around RocksDB databases.
  *
  * @param dirPath directory where LMDB files are stored.
  * @return RocksDB store manager.
  */
class RocksDbStoreManagerImpl[F[_]: Concurrent: Log](
    dirPath: Path
) extends KeyValueStoreManager[F] {

  RocksDB.loadLibrary

  private val options = new Options()
    .setCreateIfMissing(true)
    .setWriteBufferSize(256 * SizeUnit.MB)
    .setDbWriteBufferSize(1 * SizeUnit.GB)
    .setMaxWriteBufferNumber(4)
    .setCompressionType(CompressionType.SNAPPY_COMPRESSION)
//    .setCompressionType(CompressionType.DISABLE_COMPRESSION_OPTION)
    .setCompactionStyle(CompactionStyle.UNIVERSAL)

  private case class DbState(
      dbs: Map[String, Deferred[F, RocksDB]] = Map.empty
  )

  // Internal manager state for LMDB databases and environments
  private val varState = Ref.unsafe(DbState())

  override def store(dbName: String): F[KeyValueStore[F]] =
    openDatabase(dbName) map (RocksDbKeyValueStore(_))

  private def openDatabase(dbName: String): F[RocksDB] =
    for {
      // Find DB ref / first time create deferred object.
      newDbDefer <- Deferred[F, RocksDB]
      action <- varState.modify { st =>
                 val maybeDbRef = st.dbs.get(dbName)
                 maybeDbRef.fold {
                   val newDbs = st.dbs.updated(dbName, newDbDefer)
                   val newSt  = st.copy(dbs = newDbs)
                   (newSt, (true, newDbDefer))
                 }(defer => (st, (false, defer)))
               }

      // If database is not found, create new Deferred object to block
      // callers until database is created and Deferred object completed.
      (isNewDb, dbEnvDefer) = action
      _ <- (for {
            _      <- Log[F].debug(s"Creating RocksDB database: $dbName, path: $dirPath")
            _      <- Sync[F].delay(Files.createDirectories(dirPath))
            dbPath = dirPath.resolve(dbName)
            // Create RocksDB database
            db <- Sync[F].delay(RocksDB.open(options, dbPath.toFile.getAbsolutePath))
            _  <- dbEnvDefer.complete(db)
          } yield ()).whenA(isNewDb)

      // Block until database is ready.
      rdb <- dbEnvDefer.get
    } yield rdb

  override def shutdown: F[Unit] = {
    import cats.instances.list._
    for {
      // Close LMDB environment
      st      <- varState.get
      openDbs <- st.dbs.values.toList.traverse(_.get)
      _ <- openDbs.traverse_ { db =>
            Sync[F].delay {
              println(s"Closing RocksDB database.")
              db.syncWal()
              db.close()
            }
          }
    } yield ()
  }
}
