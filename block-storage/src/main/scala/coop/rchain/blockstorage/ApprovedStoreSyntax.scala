package coop.rchain.blockstorage

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.casper.protocol.{ApprovedBlock, ApprovedBlockProto}
import coop.rchain.shared.syntax._
import scodec.bits.ByteVector

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

  def getApprovedBlock: F[Option[ApprovedBlock]] = {
    import cats.instances.option._
    for {
      // Optional block message from the store
      byteVector <- approvedStore.get1(approvedBlockKey)
      // Decode protobuf message / throw if fail
      block <- byteVector.traverse { approvedBlockAsByteVector =>
                ApprovedBlock
                  .from(ApprovedBlockProto.parseFrom(approvedBlockAsByteVector.toArray))
                  .leftMap(errorApprovedBlock)
                  .liftTo[F]
              }
    } yield block
  }

  def putApprovedBlock(block: ApprovedBlock): F[Unit] =
    approvedStore.put(approvedBlockKey, ByteVector(block.toProto.toByteArray))

  private def errorApprovedBlock(cause: String): ApprovedStoreFatalError = ApprovedStoreFatalError(
    s"Approved block decoding error. Cause: $cause"
  )

  /**
    * This is fatal error from which Approved Store can not be recovered.
    */
  case class ApprovedStoreFatalError(message: String) extends Exception(message)
}
