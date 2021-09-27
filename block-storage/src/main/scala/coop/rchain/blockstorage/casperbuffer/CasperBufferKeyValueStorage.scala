package coop.rchain.blockstorage.casperbuffer

import cats.effect.Concurrent
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.dag.state.BlockDagBufferState._
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.codecs.{discriminated, provide, uint2}

object CasperBufferKeyValueStorage {
  type CasperBufferStorage[F[_]] = KeyValueTypedStore[F, BlockHash, MessageStatus]

  val codecMessageStatus = discriminated[MessageStatus]
    .by(uint2)
    .subcaseP(0) {
      case v: Requested.type => v
    }(provide(Requested))
    .subcaseP(1) {
      case v: ValidationInProgress.type => v
    }(provide(ValidationInProgress))
    .subcaseP(2) {
      case v: AwaitingDependencies => v
    }(codecBlockHashSet.as[AwaitingDependencies])

  def create[F[_]: Concurrent: Log: Metrics](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, BlockHash, MessageStatus]] =
    kvm.database[BlockHash, MessageStatus](
      "casper-buffer",
      codecBlockHash,
      codecMessageStatus
    )
}
