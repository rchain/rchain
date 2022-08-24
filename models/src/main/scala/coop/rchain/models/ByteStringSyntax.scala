package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.shared.Base16
import scodec.bits.ByteVector
import coop.rchain.rspace.hashing.Blake2b256Hash

import java.nio.ByteBuffer

trait ByteStringSyntax {
  implicit final def modelsSyntaxByteString(bs: ByteString): ByteStringOps =
    new ByteStringOps(bs)

  implicit def ordering: Ordering[ByteString] =
    Ordering.by((b: ByteString) => b.toByteArray.toIterable)

  implicit val showByteString = new Show[ByteString] {
    def show(validator: ByteString): String = validator.toHexString
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

  def toHexString: String = Base16.encode(bs.toByteArray)

  def toByteVector: ByteVector = ByteVector(bs.toByteArray)

  def toBlake2b256Hash: Blake2b256Hash = Blake2b256Hash.fromByteString(bs)

}
