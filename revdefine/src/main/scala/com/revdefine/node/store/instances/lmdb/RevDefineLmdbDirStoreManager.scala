package com.revdefine.node.store.instances.lmdb

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.LmdbDirStoreManager.{Db, LmdbEnvConfig}
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}

import java.nio.file.Path

object RevDefineLmdbDirStoreManager {
  // TODO: Return instance as Resource with the call to _shutdown_.
  //  Shutdown can also be removed from the interface and be only
  //  implemented as instance method if applicable.
  def apply[F[_]: Concurrent: Log](
      dirPath: Path,
      dbInstanceMapping: Map[Db, LmdbEnvConfig]
  ): F[KeyValueStoreManager[F]] =
    Sync[F].delay(new RevDefineLmdbDirStoreManager(dirPath, dbInstanceMapping))

  // Mega, giga and tera bytes
  val mb = 1024L * 1024L
  val gb = 1024L * mb
  val tb = 1024L * gb

}

// The idea for this class is to manage multiple of key-value lmdb databases.
// For LMDB this allows control which databases are part of the same environment (file).
private final case class RevDefineLmdbDirStoreManager[F[_]: Concurrent: Log](
    dirPath: Path,
    dbMapping: Map[Db, LmdbEnvConfig]
) extends KeyValueStoreManager[F] {

  private case class StoreState(
      envs: Map[String, Deferred[F, KeyValueStoreManager[F]]]
  )

  private val managersState = Ref.unsafe(StoreState(Map.empty))

  private val dbInstanceMapping = dbMapping.map { case (db, cfg) => (db.id, (db, cfg)) }

  // Creates database uniquely defined by the name
  override def store(dbName: String): F[KeyValueStore[F]] =
    for {
      // Find DB ref / first time create deferred object
      newDefer <- Deferred[F, KeyValueStoreManager[F]]
      action <- managersState.modify { st =>
                 val (db, cfg @ LmdbEnvConfig(manName, _)) = dbInstanceMapping(dbName)
                 val (isNew, defer)                        = st.envs.get(manName).fold((true, newDefer))((false, _))
                 val newSt                                 = st.copy(envs = st.envs.updated(manName, defer))
                 (newSt, (isNew, defer, db, cfg))
               }
      (isNew, defer, db, manCfg) = action
      _                          <- createLmdbManger(manCfg, defer).whenA(isNew)
      manager                    <- defer.get
      dataBaseName               = db.nameOverride.getOrElse(db.id)
      database                   <- manager.store(dataBaseName)
    } yield database

  private def createLmdbManger(config: LmdbEnvConfig, defer: Deferred[F, KeyValueStoreManager[F]]) =
    for {
      manager <- DefineLmdbStoreManager(dirPath.resolve(config.name), config.maxEnvSize)
      _       <- defer.complete(manager)
    } yield ()

  override def shutdown: F[Unit] = {
    import cats.instances.vector._
    for {
      st <- managersState.get
      // Shutdown all store managers
      _ <- st.envs.values.toVector.traverse_(_.get.flatMap(_.shutdown))
    } yield ()
  }
}
