package coop.rchain.rspace

import coop.rchain.rspace.internal._
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

class ScodecSerializationTests extends FlatSpec with Matchers {

  "List[DatumBytes]" should "round-trip" in {
    val bll =
      List(DatumBytes(ByteVector(Array[Byte](1, 2, 3)), false),
           DatumBytes(ByteVector(Array[Byte](4, 5, 6)), true))

    val arr = toBitVector(bll, codecSeqDatumBytes)
    val res = fromBitVector(arr, codecSeqDatumBytes)

    res shouldBe bll
  }

  "DatumBytes" should "round-trip" in {
    val asd = DatumBytes(ByteVector(Array[Byte](4, 5, 6)), true)

    val arr = toBitVector(asd, codecDatumBytes)
    val res = fromBitVector(arr, codecDatumBytes)

    res shouldBe asd
  }

  "WaitingContinuationBytes" should "round-trip" in {
    val psks = List(
      WaitingContinuationBytes(List(ByteVector(Array[Byte](1, 2, 3))),
                               ByteVector(Array[Byte](4, 5, 6)),
                               true))

    val arr = toBitVector(psks, codecSeqWaitingContinuationBytes)
    val res = fromBitVector(arr, codecSeqWaitingContinuationBytes)

    res shouldBe psks
  }
}
