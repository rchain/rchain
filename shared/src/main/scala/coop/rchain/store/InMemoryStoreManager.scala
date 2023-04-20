package coop.rchain.store

import cats.effect.Sync
import cats.syntax.all._

import scala.collection.concurrent.TrieMap

// Simple in-memory key value store manager
final case class InMemoryStoreManager[F[_]: Sync]() extends KeyValueStoreManager[F] {

  val state = TrieMap[String, InMemoryKeyValueStore[F]]()

  // Creates new database for each unique database name
  override def store(name: String): F[KeyValueStore[F]] =
    Sync[F].delay(state.getOrElseUpdate(name, InMemoryKeyValueStore[F]()))

  override def shutdown: F[Unit] = ().pure
}
