package coop.rchain.store

import cats.effect.Sync

import scala.collection.concurrent.TrieMap

// Simple in-memory key value store manager
final case class InMemoryStoreManager[F[_]: Sync]() extends KeyValueStoreManager[F] {

  val state = TrieMap[String, InMemoryKeyValueStore[F]]()

  // Creates new database for each unique database name
  override def database(name: String): F[KeyValueStore[F]] =
    Sync[F].delay(state.getOrElseUpdate(name, InMemoryKeyValueStore[F]))
}
