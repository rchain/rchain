package coop.rchain.rspace.history

import scodec.Codec
import scodec.codecs._
import scodec.bits.ByteVector

import java.lang.{Byte => JByte}

class TestKey4 private (val bytes: ByteVector) {

  require(bytes.length == TestKey4.length)

  override def toString: String =
    s"TestKey4(bytes: ${bytes.toSeq.map(JByte.toUnsignedInt).toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case tk: TestKey4 => tk.bytes === bytes
    case _            => false
  }

  override def hashCode(): Int =
    bytes.hashCode
}

object TestKey4 {

  val length: Long = 4

  def create(bytes: Seq[Int]): TestKey4 =
    new TestKey4(ByteVector(bytes.map(_.toByte)))

  implicit val codecTestKey: Codec[TestKey4] =
    fixedSizeBytes(length.toLong, bytes).as[TestKey4]
}

class TestKey5 private (val bytes: ByteVector) {

  require(bytes.length == TestKey5.length)

  override def toString: String =
    s"TestKey5(bytes: ${bytes.toSeq.map(JByte.toUnsignedInt).toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case tk: TestKey5 => tk.bytes === bytes
    case _            => false
  }

  override def hashCode(): Int =
    bytes.hashCode
}

object TestKey5 {

  val length: Long = 5

  def create(bytes: Seq[Int]): TestKey5 =
    new TestKey5(ByteVector(bytes.map(_.toByte)))

  implicit val codecTestKey: Codec[TestKey5] =
    fixedSizeBytes(length.toLong, bytes).as[TestKey5]
}
