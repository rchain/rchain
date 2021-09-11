package coop.rchain.rspace.store

import cats.Monad
import cats.syntax.all._
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.store.{KeyValueStoreManager, NoOpKeyValueStore}

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
    getStores("rspace")

  /**
    * Create stores used in Rholang evaluator
    */
  def evalStores(implicit m: Monad[F]): F[RSpaceStore[F]] =
    getStores("eval")

  private def getStores(dbPrefix: String)(
      implicit m: Monad[F]
  ): F[RSpaceStore[F]] =
    for {
      history <- kvm.store(s"$dbPrefix-history")
      roots   <- kvm.store(s"$dbPrefix-roots")
      cold    <- kvm.store(s"$dbPrefix-cold")
    } yield RSpaceStore[F](history, roots, cold)
}
