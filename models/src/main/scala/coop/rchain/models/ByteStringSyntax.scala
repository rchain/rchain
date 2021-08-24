package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector

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
  def base16String: String = Base16.encode(bs.toByteArray)

  def toByteVector: ByteVector = ByteVector(bs.toByteArray)

  def toBlake2b256Hash: Blake2b256Hash = Blake2b256Hash.fromByteString(bs)
}
