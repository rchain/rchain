package coop.rchain.shared
import cats.effect.Concurrent
import cats.syntax.all._
import cats.effect.concurrent.{Deferred, Ref}

object Caching {
  def memoize[F[_]: Concurrent, K, V](
      f: K => F[Option[V]],
      cache: Ref[F, Map[K, Deferred[F, Option[V]]]],
      cacheSize: Int = 1000
  )(key: K): F[Option[V]] =
    for {
      nDef <- Deferred[F, Option[V]]
      r <- cache.modify { s =>
            val curVal = s.get(key)
            curVal.map(v => (s, (v, true))).getOrElse((s.updated(key, nDef), (nDef, false)))
          }
      (df, complete) = r
      _ <- f(key)
            .flatMap(v => nDef.complete(v) >> cache.update(_ - key).whenA(v.isEmpty))
            .unlessA(complete)
      _ <- cache.update(s => if (s.size > cacheSize) s.take(s.size / 2) else s)
      r <- df.get
    } yield r
}
