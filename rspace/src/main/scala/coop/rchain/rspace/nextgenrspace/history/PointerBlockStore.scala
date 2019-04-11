package coop.rchain.rspace.nextgenrspace.history
import coop.rchain.rspace.Blake2b256Hash

trait PointerBlockStore[F[_]] {
  def put(key: Blake2b256Hash, pb: PointerBlock): F[Unit]

  def get(key: Blake2b256Hash): F[Option[PointerBlock]]
}
