package coop.rchain.casper.storage

import java.nio.file.Path

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager}

object RNodeKeyValueStoreManager {
  def apply[F[_]: Concurrent: Log](dirPath: Path): F[KeyValueStoreManager[F]] =
    Concurrent[F].delay(new RNodeKeyValueStoreManager(dirPath))
}

// The idea for this class is to manage all instances of key-value databases for RNode.
// For LMDB this allows control which databases are part of the same environment (file).
private final case class RNodeKeyValueStoreManager[F[_]: Concurrent: Log](
    dirPath: Path
) extends KeyValueStoreManager[F] {

  // Database name to store instance name mapping (subfolder for LMDB store)
  // - keys with the same instance will be in one LMDB file (environment)
  val dbInstanceMapping: Map[String, String] = Map[String, String](
    // Block store
    ("block-metadata", "dagstorage")
  )

  private case class StoreState(
      envs: Map[String, Deferred[F, KeyValueStoreManager[F]]]
  )

  private val managersState = Ref.unsafe(StoreState(Map.empty))

  // Creates database uniquely defined by the name
  override def database(dbName: String): F[KeyValueStore[F]] =
    for {
      // Find DB ref / first time create deferred object
      newDefer <- Deferred[F, KeyValueStoreManager[F]]
      action <- managersState.modify { st =>
                 val manName        = dbInstanceMapping(dbName)
                 val (isNew, defer) = st.envs.get(manName).fold((true, newDefer))((false, _))
                 val newSt          = st.copy(envs = st.envs.updated(manName, defer))
                 (newSt, (isNew, defer, manName))
               }
      (isNew, defer, manName) = action
      _                       <- createLmdbManger(manName, defer).whenA(isNew)
      manager                 <- defer.get
      database                <- manager.database(dbName)
    } yield database

  private def createLmdbManger(name: String, defer: Deferred[F, KeyValueStoreManager[F]]) =
    for {
      manager <- LmdbStoreManager(dirPath.resolve(name))
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
