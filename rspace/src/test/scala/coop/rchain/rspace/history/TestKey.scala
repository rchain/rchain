package coop.rchain.rspace.history

import scodec.Codec
import scodec.codecs._
import scodec.bits.ByteVector

import java.lang.{Byte => JByte}

class TestKey private (val bytes: ByteVector) {

  require(bytes.length == TestKey.length)

  override def toString: String =
    s"TestKey(bytes: ${bytes.toSeq.map(JByte.toUnsignedInt).toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case tk: TestKey => tk.bytes === bytes
    case _           => false
  }

  override def hashCode(): Int =
    bytes.hashCode
}

object TestKey {

  val length: Int = 4

  def create(bytes: Seq[Int]): TestKey =
    new TestKey(ByteVector(bytes.map(_.toByte)))

  implicit val codecTestKey: Codec[TestKey] =
    fixedSizeBytes(length.toLong, bytes).as[TestKey]
}
