package coop.rchain.rspace.state.instances

import java.nio.ByteBuffer

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history.{RootsStoreInstances, Store}
import coop.rchain.rspace.state.RSpaceImporter

object RSpaceImporterImpl {
  // RSpace importer constructor / smart constructor "guards" private class
  def apply[F[_]: Concurrent](
      historyStore: Store[F],
      valueStore: Store[F],
      rootsStore: Store[F]
  ): RSpaceImporter[F] =
    RSpaceImporterImpl(historyStore, valueStore, rootsStore)

  private final case class RSpaceImporterImpl[F[_]: Sync](
      sourceHistoryStore: Store[F],
      sourceValueStore: Store[F],
      sourceRootsStore: Store[F]
  ) extends RSpaceImporter[F] {
    override def setHistoryItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] =
      sourceHistoryStore.put(data, toBuffer)

    override def setDataItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] = sourceValueStore.put(data, toBuffer)

    override def setRoot(key: Blake2b256Hash): F[Unit] = {
      val roots = RootsStoreInstances.rootsStore(sourceRootsStore)
      roots.recordRoot(key)
    }
  }
}
