package coop.rchain.store

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.syntax._
import scodec.Codec

trait KeyValueStoreManagerSyntax {
  implicit final def sharedSyntaxKeyValueStoreManager[F[_]: Sync](
      manager: KeyValueStoreManager[F]
  ): KeyValueStoreManagerOps[F] = new KeyValueStoreManagerOps[F](manager)
}

final class KeyValueStoreManagerOps[F[_]: Sync](
    // KeyValueStoreManager extensions / syntax
    private val manager: KeyValueStoreManager[F]
) {
  def database[K, V](
      name: String,
      kCodec: Codec[K],
      vCodec: Codec[V]
  ): F[KeyValueTypedStore[F, K, V]] =
    manager.store(name).map(_.toTypedStore(kCodec, vCodec))
}
