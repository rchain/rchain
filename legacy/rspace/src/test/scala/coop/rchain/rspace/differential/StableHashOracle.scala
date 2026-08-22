package coop.rchain.rspace.differential

import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector

/**
  * Emits Scala ground-truth stable-hash vectors consumed by the Rust differential tests in
  * `crates/rspace/testdata/differential/stable_hash.tsv`.
  *
  * Run via sbt (CI only; the dev environment has no sbt):
  *   sbt "rspace/Test/runMain coop.rchain.rspace.differential.StableHashOracle"
  */
object StableHashOracle extends App {
  implicit val byteSerialize: Serialize[Vector[Byte]] = new Serialize[Vector[Byte]] {
    def encode(a: Vector[Byte]): ByteVector = ByteVector(a.toArray)
    def decode(bytes: ByteVector): Either[Throwable, Vector[Byte]] = Right(bytes.toArray.toVector)
  }

  def emit(id: String, h: Blake2b256Hash): Unit = println(s"$id\t${h.bytes.toHex}")

  emit("ch_0102", StableHashProvider.hash[Vector[Byte]](Vector(0x01.toByte, 0x02.toByte)))
  emit("ch_empty", StableHashProvider.hash[Vector[Byte]](Vector.empty))

  emit(
    "join_ab",
    StableHashProvider.hash[Vector[Byte]](Seq(Vector(0x01.toByte), Vector(0x02.toByte)))
  )

  emit(
    "produce_0102_03_false",
    StableHashProvider.hash[Vector[Byte]](ByteVector(0x01.toByte, 0x02.toByte), Vector(0x03.toByte), false)
  )
  emit(
    "produce_0102_03_true",
    StableHashProvider.hash[Vector[Byte]](ByteVector(0x01.toByte, 0x02.toByte), Vector(0x03.toByte), true)
  )

  emit(
    "consume_1ch_false",
    StableHashProvider.hash[Vector[Byte], Vector[Byte]](
      Seq(StableHashProvider.hash[Vector[Byte]](Vector(0x01.toByte)).bytes),
      Seq(Vector(0x04.toByte)),
      Vector(0x05.toByte),
      false
    )
  )

  emit(
    "consume_2ch_true",
    StableHashProvider.hash[Vector[Byte], Vector[Byte]](
      StableHashProvider
        .hashSeq[Vector[Byte]](Seq(Vector(0x01.toByte), Vector(0x02.toByte)))
        .map(_.bytes),
      Seq(Vector(0x04.toByte), Vector(0x05.toByte)),
      Vector(0x06.toByte),
      true
    )
  )
}
