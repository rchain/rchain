package coop.rchain.rspace.nextgenrspace.history
import coop.rchain.rspace.Blake2b256Hash

trait PointerBlockStore {
  def put(key: Blake2b256Hash, pb: PointerBlock): Unit

  def get(key: Blake2b256Hash): Option[PointerBlock]
}
