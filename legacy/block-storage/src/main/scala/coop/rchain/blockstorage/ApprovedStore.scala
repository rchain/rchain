package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.codecs.codecFringe
import coop.rchain.casper.protocol.{FinalizedFringe, FinalizedFringeProto}
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.codecs._

object approvedStore {
  type ApprovedStore[F[_]] = KeyValueTypedStore[F, Byte, FinalizedFringe]

  def ApprovedStore[F[_]](implicit instance: ApprovedStore[F]): instance.type = instance

  def create[F[_]: Sync](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, Byte, FinalizedFringe]] =
    kvm
      .store("finalized-store")
      .map(_.toTypedStore[Byte, FinalizedFringe](byte, codecFringe))

  def bytesToFringe(bytes: Array[Byte]): FinalizedFringe =
    FinalizedFringe.from(FinalizedFringeProto.parseFrom(bytes))

  def fringeToBytes(finalizedFringe: FinalizedFringe): Array[Byte] =
    finalizedFringe.toProto.toByteArray
}
