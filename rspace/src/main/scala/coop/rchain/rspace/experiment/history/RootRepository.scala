package coop.rchain.rspace
package experiment
package history

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import History.emptyRootHash

import scala.Function._

class RootRepository[F[_]: Sync](
    rootsStore: RootsStore[F]
) {
  val unknownRoot = new RuntimeException("unknown root")

  def commit(root: Blake2b256Hash): F[Unit] =
    rootsStore.recordRoot(root)

  def currentRoot(): F[Blake2b256Hash] =
    rootsStore.currentRoot().flatMap {
      case None       => rootsStore.recordRoot(emptyRootHash).map(const(emptyRootHash))
      case Some(root) => Applicative[F].pure(root)
    }

  def validateRoot(root: Blake2b256Hash): F[Unit] =
    rootsStore.validateRoot(root).flatMap {
      case None    => Sync[F].raiseError[Unit](unknownRoot)
      case Some(_) => Applicative[F].pure(())
    }

  def close(): F[Unit] = rootsStore.close()
}
