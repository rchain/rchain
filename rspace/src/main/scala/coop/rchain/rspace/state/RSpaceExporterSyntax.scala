package coop.rchain.rspace.state

import cats.effect.{Async, Sync}
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.state.exporters.RSpaceExporterItems.StoreItems
import coop.rchain.rspace.state.exporters.{RSpaceExporterDisk, RSpaceExporterItems}
import coop.rchain.shared.{Log, Serialize}

import java.nio.ByteBuffer
import java.nio.file.Path

trait RSpaceExporterSyntax {
  implicit final def rspaceSyntaxRSpaceExporter[F[_]](
      exporter: RSpaceExporter[F]
  ): RSpaceExporterOps[F] = new RSpaceExporterOps[F](exporter)
}

final class RSpaceExporterOps[F[_]](
    // RSpaceExporter extensions / syntax
    private val exporter: RSpaceExporter[F]
) extends AnyVal {
  // Base export operations

  def getHistory[Value](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  )(implicit m: Sync[F], l: Log[F]): F[StoreItems[Blake2b256Hash, Value]] =
    RSpaceExporterItems.getHistory(exporter, startPath, skip, take, fromBuffer)

  def getData[Value](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  )(implicit m: Sync[F], l: Log[F]): F[StoreItems[Blake2b256Hash, Value]] =
    RSpaceExporterItems.getData(exporter, startPath, skip, take, fromBuffer)

  def getHistoryAndData[Value](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  )(
      implicit m: Sync[F],
      l: Log[F]
  ): F[(StoreItems[Blake2b256Hash, Value], StoreItems[Blake2b256Hash, Value])] =
    RSpaceExporterItems.getHistoryAndData(exporter, startPath, skip, take, fromBuffer)

  // Export to disk

  def writeToDisk[C, P, A, K](root: Blake2b256Hash, dirPath: Path, chunkSize: Int)(
      implicit m: Async[F],
      l: Log[F]
  ): F[Unit] =
    RSpaceExporterDisk.writeToDisk[F](exporter, root, dirPath, chunkSize)
}
