package coop.rchain.rspace.store

import cats.Monad
import cats.syntax.all._
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.store.KeyValueStoreManager

trait RSpaceStoreManagerSyntax {
  implicit final def rspaceSyntaxKeyValueStoreManager[F[_]](
      kvm: KeyValueStoreManager[F]
  ): RSpaceStoreManagerOps[F] = new RSpaceStoreManagerOps[F](kvm)

}

final class RSpaceStoreManagerOps[F[_]](
    // KeyValueStoreManager extensions / syntax
    private val kvm: KeyValueStoreManager[F]
) extends AnyVal {

  /**
    * Create stores used in RSpace
    */
  def rSpaceStores(implicit m: Monad[F]): F[RSpaceStore[F]] =
    for {
      history  <- kvm.store("rspace-history")
      roots    <- kvm.store("rspace-roots")
      cold     <- kvm.store("rspace-cold")
      channels <- kvm.store("channels")
    } yield RSpaceStore[F](history, roots, cold, channels)

  /**
    * Create stores used in Rholang evaluator
    */
  def evalStores(implicit m: Monad[F]): F[RSpaceStore[F]] =
    for {
      history  <- kvm.store("eval-history")
      roots    <- kvm.store("eval-roots")
      cold     <- kvm.store("eval-cold")
      channels <- kvm.store("channels")
    } yield RSpaceStore[F](history, roots, cold, channels)
}
