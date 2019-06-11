package coop.rchain.rspace.nextgenrspace.history

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History.emptyRootHash

import scala.Function._

class RootRepository[F[_]: Sync](
    rootsStore: RootsStore[F]
) {

  def commit(root: Blake2b256Hash): F[Unit] =
    rootsStore.recordRoot(root)

  def currentRoot(): F[Blake2b256Hash] =
    rootsStore.currentRoot().flatMap {
      case None       => rootsStore.recordRoot(emptyRootHash).map(const(emptyRootHash))
      case Some(root) => Applicative[F].pure(root)
    }

  def validateRoot(root: Blake2b256Hash): F[Unit] =
    rootsStore.recordRoot(root)

  def close(): F[Unit] = rootsStore.close()
}
