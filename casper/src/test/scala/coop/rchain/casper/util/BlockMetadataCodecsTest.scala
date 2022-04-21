package coop.rchain.casper.util

import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Justification
import coop.rchain.models.{
  BlockMetadata,
  BlockMetadataAB,
  BlockMetadataBS,
  BlockMetadataBV,
  JustificationBA,
  JustificationBS,
  JustificationBV
}
import coop.rchain.shared.Base16
import monix.eval.Task
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.ByteVector

import scala.concurrent.duration.Duration

object GeneratorBlockMetadata {
  private def arrayToByteString(arrInt: Array[Byte]) = ByteString.copyFrom(arrInt)
  def randomArray(size: Int): Array[Byte] = {
    val r   = scala.util.Random
    val arr = new Array[Byte](size)
    for (i <- arr.indices) arr(i) = r.nextInt(255).toByte
    arr
  }
  def randomBlockMetadata(
      parentsCount: Int,
      justCount: Int,
      weightCount: Int
  ): BlockMetadata = {
    val blockHash = arrayToByteString(randomArray(size = 32))
    val parents =
      (0 until parentsCount).toList.map(_ => arrayToByteString(randomArray(size = 32)))
    val sender = arrayToByteString(randomArray(size = 10))
    val justifications = (0 until justCount).toList.map(
      _ =>
        Justification(
          arrayToByteString(randomArray(size = 32)),
          arrayToByteString(randomArray(size = 22))
        )
    )

    val weightMap = (0 until weightCount).toList
      .map(_ => arrayToByteString(randomArray(size = 11)) -> 11112L)
      .toMap

    BlockMetadata(
      blockHash,
      parents,
      sender,
      justifications,
      weightMap,
      blockNum = 123456L,
      seqNum = 11,
      invalid = false,
      directlyFinalized = false,
      finalized = false
    )
  }

  def randomBlockMetadataAB(
      parentsCount: Int,
      justCount: Int,
      weightCount: Int
  ): BlockMetadataAB =
    toBlockMetadataAB(randomBlockMetadata(parentsCount, justCount, weightCount))

  def toBlockMetadataAB(block: BlockMetadata): BlockMetadataAB =
    BlockMetadataAB(
      block.blockHash.toByteArray,
      block.parents.map(parent => parent.toByteArray),
      block.sender.toByteArray,
      block.justifications.map(
        just =>
          JustificationBA(
            just.validator.toByteArray,
            just.latestBlockHash.toByteArray
          )
      ),
      block.weightMap.map { case (k, v) => (k.toByteArray -> v) },
      block.blockNum,
      block.seqNum,
      block.invalid,
      block.directlyFinalized,
      block.finalized
    )

  def toBlockMetadataBV(block: BlockMetadata): BlockMetadataBV =
    BlockMetadataBV(
      ByteVector(block.blockHash.toByteArray),
      block.parents.map(parent => ByteVector(parent.toByteArray)),
      ByteVector(block.sender.toByteArray),
      block.justifications.map(
        just =>
          JustificationBV(
            ByteVector(just.validator.toByteArray),
            ByteVector(just.latestBlockHash.toByteArray)
          )
      ),
      block.weightMap.map { case (k, v) => (ByteVector(k.toByteArray) -> v) },
      block.blockNum,
      block.seqNum,
      block.invalid,
      block.directlyFinalized,
      block.finalized
    )

  def randomBlockMetadataBV(parentsCount: Int, justCount: Int, weightCount: Int): BlockMetadataBV =
    toBlockMetadataBV(randomBlockMetadata(parentsCount, justCount, weightCount))

  def toBlockMetadataBS(block: BlockMetadata): BlockMetadataBS =
    BlockMetadataBS(
      ByteString.copyFrom(block.blockHash.toByteArray),
      block.parents.map(parent => ByteString.copyFrom(parent.toByteArray)),
      ByteString.copyFrom(block.sender.toByteArray),
      block.justifications.map(
        just =>
          JustificationBS(
            ByteString.copyFrom(just.validator.toByteArray),
            ByteString.copyFrom(just.latestBlockHash.toByteArray)
          )
      ),
      block.weightMap.map { case (k, v) => (ByteString.copyFrom(k.toByteArray) -> v) },
      block.blockNum,
      block.seqNum,
      block.invalid,
      block.directlyFinalized,
      block.finalized
    )

  def randomBlockMetadataBS(parentsCount: Int, justCount: Int, weightCount: Int): BlockMetadataBS =
    toBlockMetadataBS(randomBlockMetadata(parentsCount, justCount, weightCount))
}

class BlockMetadataCodecsTest extends FlatSpec {
  import GeneratorBlockMetadata._
  "encode BlockMetadataScodec object with new codec and decode serialized data" should "give initial object" in {
    val testBlockMetadataScodec = randomBlockMetadataAB(
      parentsCount = 10,
      justCount = 10,
      weightCount = 100
    )

    val testBlockMetadataScodec2 = randomBlockMetadataAB(
      parentsCount = 10,
      justCount = 20,
      weightCount = 5
    )

    val referenceBlocks = Vector(testBlockMetadataScodec, testBlockMetadataScodec2)
    val serialized      = referenceBlocks.map(BlockMetadataAB.toByteVector)
    val reconstructed   = serialized.map(BlockMetadataAB.fromByteVector)

    val areEqual = reconstructed.zip(referenceBlocks).forall { case (a, b) => a.isEqualTo(b) }
    areEqual shouldBe true
  }

  "encode BlockMetadataScodec object with some empty fields and decode binary data" should "give initial object without errors" in {
    val referenceBlocks = List(
      randomBlockMetadataAB(parentsCount = 0, justCount = 10, weightCount = 10),
      randomBlockMetadataAB(parentsCount = 10, justCount = 0, weightCount = 10),
      randomBlockMetadataAB(parentsCount = 10, justCount = 10, weightCount = 0)
    )

    val serialized    = referenceBlocks.map(BlockMetadataAB.toByteVector)
    val reconstructed = serialized.map(BlockMetadataAB.fromByteVector)

    val areEqual = (reconstructed zip referenceBlocks).forall { case (a, b) => a.isEqualTo(b) }
    areEqual shouldBe true
  }
}
