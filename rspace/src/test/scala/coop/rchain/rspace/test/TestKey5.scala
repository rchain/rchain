package coop.rchain.rspace.test

import java.lang.{Byte => JByte}

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

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
