package coop.rchain.store

trait KeyValueStoreManager[F[_]] {
  def database(name: String): F[KeyValueStore[F]]

  def shutdown: F[Unit]
}

object KeyValueStoreManager {
  def apply[F[_]](implicit kvm: KeyValueStoreManager[F]): KeyValueStoreManager[F] = kvm
}
