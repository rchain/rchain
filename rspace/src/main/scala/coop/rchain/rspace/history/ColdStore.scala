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

  def coldStore[F[_]: Sync](store: Store[F]): ColdStore[F] = new ColdStore[F] {
    private val codec = codecPersistedData

    override def put(key: Blake2b256Hash, d: PersistedData): F[Unit] =
      for {
        encoded <- codec.encode(d).get
        data    <- store.put(key, encoded)
      } yield data

    override def get(key: Blake2b256Hash): F[Option[PersistedData]] =
      for {
        maybeBytes   <- store.get(key)
        maybeDecoded <- maybeBytes.map(bytes => codec.decode(bytes).get).sequence
      } yield (maybeDecoded.map(_.value))

    override def close(): F[Unit] = store.close()

    override def put(data: List[(Blake2b256Hash, PersistedData)]): F[Unit] =
      data
        .traverse {
          case (key, data) => codec.encode(data).get.map((key, _))
        }
        .flatMap(encoded => store.put(encoded))

  }
}

sealed trait PersistedData {
  def bytes: ByteVector
}

final case class JoinsLeaf(bytes: ByteVector)         extends PersistedData
final case class DataLeaf(bytes: ByteVector)          extends PersistedData
final case class ContinuationsLeaf(bytes: ByteVector) extends PersistedData
