package coop.rchain.store

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all._
import cats.effect.{Deferred, Ref}

trait KeyValueCache[F[_], K, V] {
  def get(key: K, fallback: F[V]): F[V]
  def put(key: K, value: V): F[Unit]
  def toMap: F[Map[K, V]]
}

final case class NoOpKeyValueCache[F[_]: Applicative, K, V]() extends KeyValueCache[F, K, V] {
  override def get(key: K, fallback: F[V]): F[V] = fallback
  override def put(key: K, value: V): F[Unit]    = ().pure[F]
  override def toMap: F[Map[K, V]]               = Map.empty[K, V].pure[F]
}

class LazyAdHocKeyValueCache[F[_]: Concurrent, K, V] private[LazyAdHocKeyValueCache] (
    cache: Ref[F, Map[K, Deferred[F, V]]]
) extends KeyValueCache[F, K, V] {

  final def get(key: K, fallback: F[V]): F[V] =
    for {
      defNew <- Deferred[F, V]
      ret <- cache.modify[(Deferred[F, V], Boolean)] { s =>
              s.get(key) match {
                case Some(v) => (s, (v, false))
                case None    => (s.updated(key, defNew), (defNew, true))
              }
            }
      (d, empty) = ret
      _          <- (fallback >>= d.complete).whenA(empty)
      r          <- d.get
    } yield r

  final def put(key: K, value: V): F[Unit] = get(key, value.pure).void

  final def toMap: F[Map[K, V]] =
    for {
      m <- cache.get
      r <- m.toList.traverse(
            t =>
              t._2.get.map { v =>
                (t._1, v)
              }
          )
    } yield r.toMap
}

/**
  * Similar to [[LazyKeyValueCache]] but allows to delay initialization of populate function till get call
  */
object LazyAdHocKeyValueCache {
  def apply[F[_]: Concurrent, K, V]: F[LazyAdHocKeyValueCache[F, K, V]] =
    for {
      cache <- Ref.of[F, Map[K, Deferred[F, V]]](Map.empty[K, Deferred[F, V]])
    } yield new LazyAdHocKeyValueCache(cache)
}
