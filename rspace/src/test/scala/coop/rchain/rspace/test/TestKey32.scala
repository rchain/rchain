package coop.rchain.rspace.test

import java.lang.{Byte => JByte}

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bytes, fixedSizeBytes}

class TestKey32 private (val bytes: ByteVector) {

  require(bytes.length == TestKey32.length)

  override def toString: String =
    s"TestKey32(bytes: ${bytes.toSeq.map(JByte.toUnsignedInt).toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case tk: TestKey32 => tk.bytes === bytes
    case _             => false
  }

  override def hashCode(): Int =
    bytes.hashCode
}

object TestKey32 {

  val length: Long = 32

  def create(bytes: Seq[Int]): TestKey32 =
    new TestKey32(ByteVector(bytes.map(_.toByte)))

  implicit val codecTestKey: Codec[TestKey32] =
    fixedSizeBytes(length.toLong, bytes).as[TestKey32]
}
