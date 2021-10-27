package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.shared.Base16

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
}
