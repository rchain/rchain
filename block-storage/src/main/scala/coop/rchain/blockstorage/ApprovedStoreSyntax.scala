package coop.rchain.blockstorage

import cats.effect.Sync
import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId, toFlatMapOps}
import coop.rchain.blockstorage.ApprovedStore.ApprovedStore
import coop.rchain.blockstorage.dag.codecs.codecApprovedBlockHash
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.syntax._
import scodec.bits.BitVector

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
  def getApprovedBlock: F[Option[ApprovedBlock]] =
    approvedBlockKeyAsHash.flatMap(approvedStore.get1(_))

  def putApprovedBlock(block: ApprovedBlock): F[Unit] =
    approvedBlockKeyAsHash.flatMap(approvedStore.put(_, block))

  val approvedBlockKey: Array[Byte] = Array[Byte](42)

  private def approvedBlockKeyAsHash: F[BlockHash] =
    codecApprovedBlockHash
      .decodeValue(BitVector(approvedBlockKey))
      .fold(
        err => new Exception(err.message).raiseError[F, BlockHash],
        _.pure[F]
      )

  case class BlockStoreFatalError(message: String) extends Exception(message)
}
