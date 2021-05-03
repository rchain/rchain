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

trait ReportStore[F[_]] {
  def put(hash: ByteString, data: BlockEventInfo): F[Unit]
  def get(hash: ByteString): F[Option[BlockEventInfo]]
}

class ReportStoreImpl[F[_]: Sync](
    store: KeyValueTypedStore[F, ByteString, BitVector]
) extends ReportStore[F] {
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

object ReportStore {

  def store[F[_]: Sync](kvm: KeyValueStoreManager[F]): F[ReportStore[F]] =
    for {
      db <- kvm.database("reporting-cache", codecs.codecByteString, scodec.codecs.bits)
    } yield new ReportStoreImpl[F](db)
}
