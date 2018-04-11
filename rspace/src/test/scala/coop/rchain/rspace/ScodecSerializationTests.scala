package coop.rchain.rspace

import coop.rchain.rspace.internal._
import coop.rchain.rspace.internal.scodecs._
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

class ScodecSerializationTests extends FlatSpec with Matchers {

  "List[DatumBytes]" should "round-trip" in {
    val bll =
      List(DatumBytes(ByteVector(Array[Byte](1, 2, 3)), false),
           DatumBytes(ByteVector(Array[Byte](4, 5, 6)), true))

    val arr = toBitVector(bll, scodecs.datumBytesesCodec)
    val res = fromBitVector(arr, scodecs.datumBytesesCodec)

    res shouldBe bll
  }

  "DatumBytes" should "round-trip" in {
    val asd = DatumBytes(ByteVector(Array[Byte](4, 5, 6)), true)

    val arr = toBitVector(asd, scodecs.datumBytesCodec)
    val res = fromBitVector(arr, scodecs.datumBytesCodec)

    res shouldBe asd
  }

  "WaitingContinuationBytes" should "round-trip" in {
    val psks = List(
      WaitingContinuationBytes(List(ByteVector(Array[Byte](1, 2, 3))),
                               ByteVector(Array[Byte](4, 5, 6)),
                               true))

    val arr = toBitVector(psks, scodecs.waitingContinuationsSeqCodec)
    val res = fromBitVector(arr, scodecs.waitingContinuationsSeqCodec)

    res shouldBe psks
  }
}
