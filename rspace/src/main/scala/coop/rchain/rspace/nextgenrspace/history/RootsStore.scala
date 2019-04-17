package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash

trait RootsStore[F[_]] {
  def currentRoot(): F[Option[Blake2b256Hash]]
  def validateRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]]
  def recordRoot(key: Blake2b256Hash): F[Unit]
}
