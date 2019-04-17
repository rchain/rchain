package coop.rchain.rspace.nextgenrspace.history

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash

class RootRepository[F[_]: Sync](
    rootsStore: RootsStore[F]
) {
  def commit(root: Blake2b256Hash): F[Unit] =
    for {
      _ <- rootsStore.recordRoot(root)
    } yield ()

  def currentRoot(): F[Blake2b256Hash] =
    for {
      maybeRoot <- rootsStore.currentRoot()
      fetched <- maybeRoot match {
                  case None =>
                    // kickstart empty root
                    rootsStore.recordRoot(History.emptyRootHash).map(_ => History.emptyRootHash)
                  case Some(root: Blake2b256Hash) => Applicative[F].pure(root)
                }
    } yield fetched

  def history(root: Blake2b256Hash): F[Blake2b256Hash] =
    for {
      maybeRoot <- rootsStore.validateRoot(root)
      _ <- maybeRoot match {
            case None =>
              Sync[F].raiseError[Blake2b256Hash](new RuntimeException("unknown root"))
            case Some(root: Blake2b256Hash) => Applicative[F].pure(root)
          }
    } yield root
}
