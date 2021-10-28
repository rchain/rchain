package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.shared.Base16
import scodec.bits.ByteVector

import java.nio.ByteBuffer

trait ByteStringSyntax {
  implicit final def modelsSyntaxByteString(bs: ByteString): ByteStringOps =
    new ByteStringOps(bs)

  implicit def ordering: Ordering[ByteString] =
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)

  implicit val show = new Show[ByteString] {
    def show(validator: ByteString): String = validator.base16String
  }
}

final class ByteStringOps(
    // ByteString extensions / syntax
    private val bs: ByteString
) extends AnyVal {

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def toDirectByteBuffer: ByteBuffer = {
    val buffer: ByteBuffer = ByteBuffer.allocateDirect(bs.size)
    bs.copyTo(buffer)
    buffer.flip()
    buffer
  }

  def base16String: String = Base16.encode(bs.toByteArray)

  def toByteVector: ByteVector = ByteVector(bs.toByteArray)

}
