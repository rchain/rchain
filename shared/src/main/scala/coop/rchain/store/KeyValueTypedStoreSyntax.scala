package coop.rchain.store

import cats.Functor
import cats.syntax.all._

trait KeyValueTypedStoreSyntax {
  implicit final def sharedSyntaxKeyValueTypedStore[F[_]: Functor, K, V](
      store: KeyValueTypedStore[F, K, V]
  ): KeyValueTypedStoreOps[F, K, V] = new KeyValueTypedStoreOps[F, K, V](store)
}

final class KeyValueTypedStoreOps[F[_]: Functor, K, V](
    // KeyValueTypedStore extensions / syntax
    private val store: KeyValueTypedStore[F, K, V]
) {
  def get(key: K): F[Option[V]] = store.get(Seq(key)).map(_.head)

  def put(key: K, value: V): F[Unit] = store.put(Seq((key, value)))

  def delete(key: K): F[Boolean] = store.delete(Seq(key)).map(_ == 1)

  def contains(key: K): F[Boolean] = store.contains(Seq(key)).map(_.head)
}
