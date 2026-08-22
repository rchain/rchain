package coop.rchain.rspace.differential

import coop.rchain.rspace.serializers.ScodecSerialize
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector
import scodec.codecs.bool

/**
  * Emits Scala ground-truth scodec byte-level vectors consumed by the Rust differential tests in
  * `crates/rspace/testdata/differential/scodec.tsv`. Covers only the byte-aligned codecs; the
  * bit-packed `encodeDatums`/`encodeContinuations` are deferred pending the Rust `BitWriter` fix.
  *
  * Run via sbt (CI only):
  *   sbt "rspace/Test/runMain coop.rchain.rspace.differential.ScodecOracle"
  */
object ScodecOracle extends App {
  implicit val byteSerialize: Serialize[Vector[Byte]] = new Serialize[Vector[Byte]] {
    def encode(a: Vector[Byte]): ByteVector = ByteVector(a.toArray)
    def decode(bytes: ByteVector): Either[Throwable, Vector[Byte]] = Right(bytes.toArray.toVector)
  }

  def emit(id: String, hex: String): Unit = println(s"$id\t$hex")

  // `size_head` == `variableSizeBytesLong(int64, bytes)` == `Serialize.codecByteVector`.
  emit("size_head_0102", Serialize.codecByteVector.encode(ByteVector(0x01.toByte, 0x02.toByte)).require.toByteVector.toHex)
  emit("size_head_empty", Serialize.codecByteVector.encode(ByteVector.empty).require.toByteVector.toHex)

  emit("bool8_true", bool(8).encode(true).require.toByteVector.toHex)
  emit("bool8_false", bool(8).encode(false).require.toByteVector.toHex)

  emit(
    "seq_bv_2",
    ScodecSerialize.codecSeqByteVector
      .encode(Seq(ByteVector(0x01.toByte), ByteVector(0x02.toByte, 0x03.toByte)))
      .require
      .toByteVector
      .toHex
  )
  emit(
    "seq_bv_0",
    ScodecSerialize.codecSeqByteVector.encode(Seq.empty[ByteVector]).require.toByteVector.toHex
  )

  emit("datums_binary_unsorted", ScodecSerialize.encodeDatumsBinary(Seq(ByteVector(0x02.toByte), ByteVector(0x01.toByte))).toHex)
  emit("cont_binary_unsorted", ScodecSerialize.encodeContinuationsBinary(Seq(ByteVector(0x02.toByte), ByteVector(0x01.toByte))).toHex)
  emit("joins_binary_unsorted", ScodecSerialize.encodeJoinsBinary(Seq(ByteVector(0x02.toByte), ByteVector(0x01.toByte))).toHex)

  emit(
    "joins_nested",
    ScodecSerialize
      .encodeJoins[Vector[Byte]](
        Seq(
          Seq(Vector(0x02.toByte), Vector(0x01.toByte)),
          Seq(Vector(0x03.toByte))
        )
      )
      .toHex
  )
}
