package coop.rchain.store

import cats.Functor
import cats.effect.Sync
import cats.syntax.all._

trait KeyValueTypedStoreSyntax {
  implicit final def sharedSyntaxKeyValueTypedStore[F[_], K, V](
      store: KeyValueTypedStore[F, K, V]
  ): KeyValueTypedStoreOps[F, K, V] = new KeyValueTypedStoreOps[F, K, V](store)
}

final class KeyValueTypedStoreOps[F[_], K, V](
    // KeyValueTypedStore extensions / syntax
    private val store: KeyValueTypedStore[F, K, V]
) extends AnyVal {
  def get(key: K)(implicit f: Functor[F]): F[Option[V]] = store.get(Seq(key)).map(_.head)

  def put(key: K, value: V): F[Unit] = store.put(Seq((key, value)))

  def putIfAbsent(kvPairs: Seq[(K, V)])(implicit s: Sync[F]) =
    for {
      ifAbsent   <- store.contains(kvPairs.map(_._1))
      kvIfAbsent = kvPairs zip ifAbsent
      kvAbsent   = kvIfAbsent.filterNot(_._2)
      _          <- store.put(kvAbsent.map(_._1))
    } yield ()

  def delete(key: K)(implicit f: Functor[F]): F[Boolean] = store.delete(Seq(key)).map(_ == 1)

  def contains(key: K)(implicit f: Functor[F]): F[Boolean] = store.contains(Seq(key)).map(_.head)

  def getOrElse(key: K, elseValue: V)(implicit f: Functor[F]): F[V] =
    get(key).map(_.getOrElse(elseValue))

  def toMap(implicit s: Sync[F]): F[Map[K, V]] =
    for {
      stream <- store.iterStream
      result <- stream.compile.toList.map(_.toMap)
    } yield result
}
