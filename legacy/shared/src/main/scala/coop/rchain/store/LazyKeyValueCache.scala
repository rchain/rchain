package coop.rchain.store

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._

class LazyKeyValueCache[F[_]: Concurrent, K, V] private[LazyKeyValueCache] (
    cache: Ref[F, Map[K, Deferred[F, V]]],
    populate: K => F[V]
) {

  final def get(key: K): F[V] =
    for {
      defNew <- Deferred[F, V]
      ret <- cache.modify[(Deferred[F, V], Boolean)] { s =>
              if (s.contains(key)) {
                (s, (s(key), false))
              } else {
                (s + (key -> defNew), (defNew, true))
              }
            }
      (d, empty) = ret
      _ <- Concurrent[F].whenA(empty)(
            populate(key) >>= d.complete
          )
      r <- d.get
    } yield r

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
  * Cache that populates value using supplied function only once per value request.
  */
object LazyKeyValueCache {
  def apply[F[_]: Concurrent, K, V](
      populateKeyWithValue: K => F[V]
  ): F[LazyKeyValueCache[F, K, V]] =
    for {
      cache <- Ref.of[F, Map[K, Deferred[F, V]]](Map.empty[K, Deferred[F, V]])
    } yield new LazyKeyValueCache(cache, populateKeyWithValue)
}
