package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.shared.Base16

trait StringSyntax {
  implicit final def modelsSyntaxString(s: String): StringOps =
    new StringOps(s)

}
class StringOps(private val s: String) extends AnyVal {
  def unsafeToByteString: ByteString      = ByteString.copyFrom(unsafeDecodeHex)
  def hexToByteString: Option[ByteString] = decodeHex.map(ByteString.copyFrom)
  def decodeHex: Option[Array[Byte]]      = Base16.decode(s)
  def unsafeDecodeHex: Array[Byte]        = Base16.unsafeDecode(s)
}
