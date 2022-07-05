package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.shared.Base16

trait ByteArraySyntax {
  implicit final def modelsSyntaxByteArray(bs: Array[Byte]): ByteArrayOps =
    new ByteArrayOps(bs)

}

class ByteArrayOps(private val ba: Array[Byte]) extends AnyVal {
  def toByteString: ByteString = ByteString.copyFrom(ba)
  def toHexString: String      = Base16.encode(ba)
}
