package coop.rchain.rspace.state.instances

import java.nio.ByteBuffer
import cats.effect.{Async, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RootsStoreInstances
import coop.rchain.rspace.state.RSpaceImporter
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

object RSpaceImporterStore {
  // RSpace importer constructor / smart constructor "guards" private class
  def apply[F[_]: Async](
      historyStore: KeyValueStore[F],
      valueStore: KeyValueStore[F],
      rootsStore: KeyValueStore[F]
  ): RSpaceImporter[F] =
    RSpaceImporterImpl(historyStore, valueStore, rootsStore)

  private final case class RSpaceImporterImpl[F[_]: Sync](
      historyStore: KeyValueStore[F],
      valueStore: KeyValueStore[F],
      rootsStore: KeyValueStore[F]
  ) extends RSpaceImporter[F] {
    val roots = RootsStoreInstances.rootsStore(rootsStore)

    override def setHistoryItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] =
      historyStore.put(data.map(_.leftMap(_.bytes.toDirectByteBuffer)), toBuffer)

    override def setDataItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] =
      valueStore.put(data.map(_.leftMap(_.bytes.toDirectByteBuffer)), toBuffer)

    override def setRoot(key: Blake2b256Hash): F[Unit] =
      roots.recordRoot(key)

    override def getHistoryItem(hash: Blake2b256Hash): F[Option[ByteVector]] =
      historyStore.get(Seq(hash.bytes.toDirectByteBuffer), ByteVector(_)).map(_.head)
  }
}
