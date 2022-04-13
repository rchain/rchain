package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.bits.ByteVector
import scodec.codecs._

object approvedStore {
  type ApprovedStore[F[_]] = KeyValueTypedStore[F, Byte, ByteVector]
  def ApprovedStore[F[_]](implicit instance: ApprovedStore[F]): instance.type = instance

  def create[F[_]: Sync](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, Byte, ByteVector]] =
    kvm
      .store("blocks-approved")
      .map(_.toTypedStore[Byte, ByteVector](byte, bytes))
}
