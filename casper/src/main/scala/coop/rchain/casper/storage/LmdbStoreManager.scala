package coop.rchain.casper.storage

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}
import enumeratum.{Enum, EnumEntry}
import org.lmdbjava.{DbiFlags, Env, EnvFlags}
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE

object LmdbStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path, maxEnvSize: Long): F[KeyValueStoreManager[F]] =
    Deferred[F, Env[ByteBuffer]] map (LmdbStoreManagerImpl(dirPath, maxEnvSize, _))
}

/**
  * Wrapper around LMDB environment which can hold multiple databases.
  *
  * @param dirPath directory where LMDB files are stored.
  * @param maxEnvSize maximum LMDB environment (file) size.
  * @param envDefer deferred object for LMDB environment in use.
  * @return LMDB store manager.
  */
private final case class LmdbStoreManagerImpl[F[_]: Concurrent: Log](
    dirPath: Path,
    maxEnvSize: Long,
    envDefer: Deferred[F, Env[ByteBuffer]]
) extends KeyValueStoreManager[F] {

  sealed trait EnvRefStatus extends EnumEntry
  object EnvRefStatus extends Enum[EnvRefStatus] {
    val values = findValues
    case object EnvClosed   extends EnvRefStatus
    case object EnvStarting extends EnvRefStatus
    case object EnvOpen     extends EnvRefStatus
    case object EnvClosing  extends EnvRefStatus
  }
  import EnvRefStatus._

  private case class DbState(
      status: EnvRefStatus,
      inProgress: Int,
      envDefer: Deferred[F, Env[ByteBuffer]],
      dbs: Map[String, Deferred[F, DbEnv[F]]] = Map.empty
  ) {
    override def toString() = s"DbState(status: $status, inProgress: $inProgress)"
  }

  // Internal manager state for LMDB databases and environments
  private val varState = Ref.unsafe(DbState(EnvClosed, 0, envDefer))

  override def store(dbName: String): F[KeyValueStore[F]] =
    Sync[F].delay(new LmdbKeyValueStore[F](getCurrentEnv(dbName)))

  private def getCurrentEnv(dbName: String): F[DbEnv[F]] =
    for {
      // Check if environment is closed
      isNewEnv <- varState.modify { st =>
                   if (st.status == EnvClosed) {
                     val newState = st.copy(status = EnvStarting)
                     (newState, true)
                   } else (st, false)
                 }
      // Create new environment when it's closed.
      // IMPORTANT: this can happen only once, all other callers
      // are blocked on Deferred object until it's completed.
      _ <- createEnv.whenA(isNewEnv)

      // Block until environment is ready.
      st  <- varState.get
      env <- st.envDefer.get

      // Find DB ref / first time create deferred object.
      newDbDefer <- Deferred[F, DbEnv[F]]
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
            _   <- Log[F].debug(s"Creating LMDB database: $dbName, env: $dirPath")
            dbi <- Sync[F].delay(env.openDbi(dbName, DbiFlags.MDB_CREATE))
            _   <- dbEnvDefer.complete(DbEnv(env, dbi, decrementDbRefCounter))
          } yield ()).whenA(isNewDb)

      // Block until database is ready.
      envDbi <- dbEnvDefer.get

      // Increment request counters to track in unfinished requests.
      _ <- incrementDbRefCounter
    } yield envDbi

  // Begin database request
  private def incrementDbRefCounter: F[Unit] =
    varState.update { st =>
      st.copy(inProgress = st.inProgress + 1)
    }

  // Finish database request
  private def decrementDbRefCounter: F[Unit] =
    varState.update { st =>
      st.copy(inProgress = st.inProgress - 1)
    }

  // Create LMDB environment and update the state
  private def createEnv: F[Unit] =
    for {
      _     <- Log[F].debug(s"Creating LMDB environment: $dirPath")
      _     <- Sync[F].delay(Files.createDirectories(dirPath))
      flags = Seq(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
      // Create environment
      env <- Sync[F].delay(
              Env
                .create(PROXY_SAFE)
                .setMapSize(maxEnvSize)
                .setMaxDbs(20)
                // Maximum parallel readers
                .setMaxReaders(2048)
                .open(dirPath.toFile, flags: _*)
            )
      // Update state to Open
      envDefer <- varState.modify { st =>
                   val newState = st.copy(status = EnvOpen)
                   (newState, newState.envDefer)
                 }
      // Complete deferred object
      _ <- envDefer.complete(env)
    } yield ()

  override def shutdown: F[Unit] =
    for {
      // Close LMDB environment
      st <- varState.get
      _  <- st.envDefer.get.map(_.close()).whenA(st.status == EnvOpen)
    } yield ()
}
