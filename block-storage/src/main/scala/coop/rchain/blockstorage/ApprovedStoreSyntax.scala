package coop.rchain.blockstorage

import cats.effect.Sync
import cats.implicits.{
  catsSyntaxApplicativeErrorId,
  catsSyntaxApplicativeId,
  toFlatMapOps,
  toFunctorOps
}
import cats.syntax.all._
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
    for {
      // Optional block message from the store
      approvedBlockHash <- approvedBlockKeyAsHash
      proto             <- approvedStore.get1(approvedBlockHash).map(_.map(_.toProto))
      // Decode protobuf message / throw if fail
      block <- proto.traverse(ApprovedBlock.from(_).leftMap(errorApprovedBlock).liftTo[F])
    } yield block

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

  private def errorApprovedBlock(cause: String): BlockStoreFatalError = BlockStoreFatalError(
    s"Approved block decoding error. Cause: $cause"
  )

  case class BlockStoreFatalError(message: String) extends Exception(message)
}
