package coop.rchain.store

import cats.effect.{Resource, Sync}
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

  /**
    * Returns typed key-value store (DB).
    *
    * @param name database name
    * @param kCodec codec for the key
    * @param vCodec codec for the value
    */
  def database[K, V](
      name: String,
      kCodec: Codec[K],
      vCodec: Codec[V]
  ): F[KeyValueTypedStore[F, K, V]] =
    manager.store(name).map(_.toTypedStore(kCodec, vCodec))

  /**
    * Wraps manager with Resource calling shutdown on exit.
    */
  def asResource: Resource[F, KeyValueStoreManager[F]] =
    Resource.make(manager.pure[F])(_.shutdown)
}
