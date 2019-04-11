package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.bits.ByteVector

trait ColdStore[F[_]] {
  def put(hash: Blake2b256Hash, d: PersistedData): F[Unit]
  def get(hash: Blake2b256Hash): F[Option[PersistedData]]
}

sealed trait PersistedData {
  def bytes: ByteVector
}
final case class JoinsLeaf(bytes: ByteVector)         extends PersistedData
final case class DataLeaf(bytes: ByteVector)          extends PersistedData
final case class ContinuationsLeaf(bytes: ByteVector) extends PersistedData
