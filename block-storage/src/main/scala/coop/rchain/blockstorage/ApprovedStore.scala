package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.codecs.codecApprovedBlock
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.codecs._

object approvedStore {
  type ApprovedStore[F[_]] = KeyValueTypedStore[F, Byte, ApprovedBlock]

  def create[F[_]: Sync](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, Byte, ApprovedBlock]] =
    kvm
      .store("blocks-approved")
      .map(_.toTypedStore[Byte, ApprovedBlock](byte, codecApprovedBlock))
}
