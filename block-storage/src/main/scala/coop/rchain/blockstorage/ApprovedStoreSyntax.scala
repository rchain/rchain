package coop.rchain.blockstorage

import cats.effect.Sync
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.shared.syntax._

trait ApprovedStoreSyntax {
  implicit final def syntaxApprovedStore[F[_]: Sync](
      approvedStore: ApprovedStore[F]
  ): ApprovedStoreOps[F] =
    new ApprovedStoreOps[F](approvedStore)
}

final class ApprovedStoreOps[F[_]: Sync](
    // ApprovedStore extensions / syntax
    private val approvedStore: ApprovedStore[F]
) {
  val approvedBlockKey: Byte = 42.toByte

  def getApprovedBlock: F[Option[ApprovedBlock]] = approvedStore.get1(approvedBlockKey)

  def putApprovedBlock(block: ApprovedBlock): F[Unit] = approvedStore.put(approvedBlockKey, block)
}
