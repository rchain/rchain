package coop.rchain.rspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.Blake2b256Hash.codecPureBlake2b256Hash
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{discriminated, uint2}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.shared.syntax._

trait ColdStore[F[_]] {
  def put(hash: Blake2b256Hash, data: PersistedData): F[Unit]
  def put(data: List[(Blake2b256Hash, PersistedData)]): F[Unit]

  def get(hash: Blake2b256Hash): F[Option[PersistedData]]

  def close(): F[Unit]
}

object ColdStoreInstances {
  type ColdKeyValueStore[F[_]] = KeyValueTypedStore[F, Blake2b256Hash, PersistedData]

  def coldStore[F[_]: Sync](store: KeyValueStore[F]): ColdKeyValueStore[F] =
    store.toTypedStore(codecPureBlake2b256Hash, codecPersistedData)

  val codecPersistedData: Codec[PersistedData] =
    discriminated[PersistedData]
      .by(uint2)
      .subcaseP(0) {
        case n: JoinsLeaf => n
      }(codecByteVector.as[JoinsLeaf])
      .subcaseP(1) {
        case s: DataLeaf => s
      }(codecByteVector.as[DataLeaf])
      .subcaseP(2) {
        case c: ContinuationsLeaf => c
      }(codecByteVector.as[ContinuationsLeaf])
}

sealed trait PersistedData {
  def bytes: ByteVector
}

final case class JoinsLeaf(bytes: ByteVector)         extends PersistedData
final case class DataLeaf(bytes: ByteVector)          extends PersistedData
final case class ContinuationsLeaf(bytes: ByteVector) extends PersistedData
