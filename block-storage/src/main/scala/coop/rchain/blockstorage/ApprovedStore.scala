package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.codecs.{codecApprovedBlock, codecBlockHash}
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

object ApprovedStore {
  type ApprovedStore[F[_]] = KeyValueTypedStore[F, BlockHash, ApprovedBlock]

  def apply[F[_]: Sync](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, BlockHash, ApprovedBlock]] =
    kvm
      .store("blocks-approved")
      .map(_.toTypedStore[BlockHash, ApprovedBlock](codecBlockHash, codecApprovedBlock))
}
