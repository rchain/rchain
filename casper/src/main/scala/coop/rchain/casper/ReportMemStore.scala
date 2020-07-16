package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs
import coop.rchain.casper.protocol.BlockEventInfo
import coop.rchain.metrics.Span
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.bits.{BitVector, ByteVector}

/**
  This is a temporary solution for the performance problem which is caused by the
  reporting api. Exchanges highly rely on this api to get transactions info.But
  every time request to the event data, the node would replay the block once again
  which makes the api so slow and can not processed concurrently.

  The ReportHotStore is a in memory store for event data in every block.
  */
trait ReportMemStore[F[_]] {
  def put(hash: ByteString, data: BlockEventInfo): F[Unit]
  def get(hash: ByteString): F[Option[BlockEventInfo]]
}

class ReportMemStoreImpl[F[_]: Sync](
    store: KeyValueTypedStore[F, ByteString, BitVector]
) extends ReportMemStore[F] {
  override def get(hash: ByteString): F[Option[BlockEventInfo]] = {
    import cats.instances.option._
    for {
      encoded <- store.get(hash)
      result  = encoded.map(e => BlockEventInfo.parseFrom(e.toByteArray))
    } yield result
  }

  override def put(hash: ByteString, data: BlockEventInfo): F[Unit] =
    for {
      encoded <- Sync[F].delay(BitVector(data.toByteArray))
      _       <- store.put(hash, encoded)
    } yield ()
}

object ReportMemStore {

  def store[F[_]: Sync: Span](
      implicit
      kvm: KeyValueStoreManager[F]
  ): F[ReportMemStore[F]] =
    for {
      db <- kvm.database("reporting-cache", codecs.codecByteString, scodec.codecs.bits)
    } yield new ReportMemStoreImpl[F](db)
}
