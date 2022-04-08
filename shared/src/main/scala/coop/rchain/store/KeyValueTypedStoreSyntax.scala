package coop.rchain.store

import cats.effect.Sync
import cats.syntax.all._
import cats.{Functor, Show}

trait KeyValueTypedStoreSyntax {
  implicit final def sharedSyntaxKeyValueTypedStore[F[_], K, V](
      store: KeyValueTypedStore[F, K, V]
  ): KeyValueTypedStoreOps[F, K, V] = new KeyValueTypedStoreOps[F, K, V](store)
}

final class KeyValueTypedStoreOps[F[_], K, V](
    // KeyValueTypedStore extensions / syntax
    private val store: KeyValueTypedStore[F, K, V]
) extends AnyVal {
  def errKVStoreExpectValue(hash: String) =
    s"Error when unsafe reading from KeyValueStore: value for key ${hash} not found."

  def get1(key: K)(implicit f: Functor[F]): F[Option[V]] = store.get(Seq(key)).map(_.head)

  def getUnsafeBatch(keys: Seq[K])(implicit f: Sync[F], show: Show[K]): F[List[V]] =
    store
      .get(keys)
      .flatMap(_.zip(keys).toList.traverse {
        case (vOpt, key) => vOpt.liftTo[F](new Exception(errKVStoreExpectValue(key.show)))
      })

  def getUnsafe(key: K)(implicit f: Sync[F], show: Show[K]): F[V] =
    get1(key).flatMap(_.liftTo[F](new Exception(errKVStoreExpectValue(key.show))))

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
    get1(key).map(_.getOrElse(elseValue))
}

object KeyValueTypedStoreSyntaxObj extends KeyValueTypedStoreSyntax
