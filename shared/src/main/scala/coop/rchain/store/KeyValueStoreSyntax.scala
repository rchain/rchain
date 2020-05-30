package coop.rchain.store

import cats.effect.Sync
import scodec.Codec

trait KeyValueStoreSyntax {
  implicit final def sharedSyntaxKeyValueStore[F[_]](store: KeyValueStore[F]): KeyValueStoreOps[F] =
    new KeyValueStoreOps[F](store)
}

final class KeyValueStoreOps[F[_]](
    // KeyValueStore extensions / syntax
    private val store: KeyValueStore[F]
) {
  // From key-value store, with serializers for K and V, typed store can be created
  def toTypedStore[K, V](kCodec: Codec[K], vCodec: Codec[V])(
      implicit s: Sync[F]
  ): KeyValueTypedStore[F, K, V] = new KeyValueTypedStoreCodec[F, K, V](store, kCodec, vCodec)
}
